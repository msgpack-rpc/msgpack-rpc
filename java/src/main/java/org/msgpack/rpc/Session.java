//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 FURUHASHI Sadayuki
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
package org.msgpack.rpc;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import org.msgpack.rpc.address.Address;
import org.msgpack.rpc.message.RequestMessage;
import org.msgpack.rpc.message.NotifyMessage;
import org.msgpack.rpc.reflect.Reflect;
import org.msgpack.rpc.transport.ClientTransport;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.type.Value;
import org.msgpack.type.ValueFactory;

public class Session {
    protected Address address;
    protected EventLoop loop;
    private ClientTransport transport;
    private Reflect reflect;

    private int requestTimeout;
    private AtomicInteger seqid = new AtomicInteger(0); // FIXME rand()?
    private Map<Integer, FutureImpl> reqtable = new HashMap<Integer, FutureImpl>();

    Session(Address address, ClientConfig config, EventLoop loop) {
        this(address,config,loop,new Reflect(loop.getMessagePack()));
    }

    Session(Address address, ClientConfig config, EventLoop loop,Reflect reflect) {
        this.address = address;
        this.loop = loop;
        this.requestTimeout = config.getRequestTimeout();
        this.transport = loop.openTransport(config, this);
        this.reflect = reflect;
    }

    public <T> T proxy(Class<T> iface) {
        return reflect.getProxy(iface).newProxyInstance(this);
        // Reflect.reflectProxy(iface,loop.getMessagePack()).newProxyInstance(this);
    }

    public Address getAddress() {
        return address;
    }

    // FIXME EventLoopHolder interface?
    public EventLoop getEventLoop() {
        return loop;
    }

    /**
     * Timeout seconds
     * @return
     */
    public int getRequestTimeout() {
        return requestTimeout;
    }

    public void setRequestTimeout(int requestTimeout) {
        this.requestTimeout = requestTimeout;
    }

    public Value callApply(String method, Object[] args) {
        Future<Value> f = sendRequest(method, args);
        while (true) {
            try {
                if(requestTimeout <= 0){
                    return f.get();
                }else{
                    return f.get(requestTimeout, TimeUnit.SECONDS);
                }
            } catch (InterruptedException e) {
                // FIXME
            } catch (TimeoutException e) {
                // FIXME
                throw new RuntimeException("Time out to call method:" + method,e);

            }
        }
    }

    public Future<Value> callAsyncApply(String method, Object[] args) {
        return sendRequest(method, args);
    }

    public void notifyApply(String method, Object[] args) {
        sendNotify(method, args);
    }

    public Future<Value> sendRequest(String method, Object[] args) {
        int msgid = seqid.getAndAdd(1);
        RequestMessage msg = new RequestMessage(msgid, method, args);
        FutureImpl f = new FutureImpl(this);

        synchronized (reqtable) {
            reqtable.put(msgid, f);
        }
        transport.sendMessage(msg);

        return new Future<Value>(loop.getMessagePack(), f);
    }

    public void sendNotify(String method, Object[] args) {
        NotifyMessage msg = new NotifyMessage(method, args);
        transport.sendMessage(msg);
    }

    void closeSession() {
        transport.close();
        synchronized (reqtable) {
            for (Map.Entry<Integer, FutureImpl> pair : reqtable.entrySet()) {
                // FIXME error result
                FutureImpl f = pair.getValue();
                f.setResult(null, ValueFactory.createRawValue("session closed"));
            }
            reqtable.clear();
        }
    }

    public void transportConnectFailed() { // FIXME error rseult
        /*
        synchronized(reqtable) {
            for(Map.Entry<Integer,FutureImpl> pair : reqtable.entrySet()) {
                // FIXME
                FutureImpl f = pair.getValue();
                f.setResult(null,null);
            }
            reqtable.clear();
        }
        */
    }

    public void onResponse(int msgid, Value result, Value error) {
        FutureImpl f;
        synchronized (reqtable) {
            f = reqtable.remove(msgid);
        }
        if (f == null) {
            // FIXME log
            return;
        }
        f.setResult(result, error);
    }

    void stepTimeout() {
        List<FutureImpl> timedout = new ArrayList<FutureImpl>();
        synchronized (reqtable) {
            for (Iterator<Map.Entry<Integer, FutureImpl>> it = reqtable
                    .entrySet().iterator(); it.hasNext();) {
                Map.Entry<Integer, FutureImpl> pair = it.next();
                FutureImpl f = pair.getValue();
                if (f.stepTimeout()) {
                    it.remove();
                    timedout.add(f);
                }
            }
        }
        for (FutureImpl f : timedout) {
            // FIXME error result
            f.setResult(null, ValueFactory.createRawValue("timedout"));
        }
    }
}
