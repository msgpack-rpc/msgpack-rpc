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
import java.util.concurrent.atomic.AtomicInteger;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.address.Address;
import org.msgpack.rpc.message.RequestMessage;
import org.msgpack.rpc.message.NotifyMessage;
import org.msgpack.rpc.reflect.ProxyBuilder;
import org.msgpack.rpc.transport.ClientTransport;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.loop.EventLoop;

public class Session {
	protected Address address;
	protected EventLoop loop;
	private ClientTransport transport;

	private int requestTimeout;
	private AtomicInteger seqid = new AtomicInteger(0);  // FIXME rand()?
	private Map<Integer, Future> reqtable = new HashMap<Integer, Future>();

	Session(Address address, ClientConfig config, EventLoop loop) {
		this.address = address;
		this.loop = loop;
		this.requestTimeout = config.getRequestTimeout();
		this.transport = loop.openTransport(config, this);
	}

	public <T> T proxy(Class<T> iface) {
		return ProxyBuilder.build(iface).newProxyInstance(this);
	}

	public Address getAddress() {
		return address;
	}

	// FIXME EventLoopHolder interface?
	public EventLoop getEventLoop() {
		return loop;
	}

	public int getRequestTimeout() {
		return requestTimeout;
	}

	public void setRequestTimeout(int requestTimeout) {
		this.requestTimeout = requestTimeout;
	}

	public MessagePackObject callApply(String method, Object[] args) {
		return sendRequest(method, args).get();
	}

	public Future callAsyncApply(String method, Object[] args) {
		return sendRequest(method, args);
	}

	public void notifyApply(String method, Object[] args) {
		sendNotify(method, args);
	}

	public Future sendRequest(String method, Object[] args) {
		int msgid = seqid.getAndAdd(1);
		RequestMessage msg = new RequestMessage(msgid, method, args);
		Future f = new Future(this);

		synchronized(reqtable) {
			reqtable.put(msgid, f);
		}
		transport.sendMessage(msg);

		return f;
	}

	public void sendNotify(String method, Object[] args) {
		NotifyMessage msg = new NotifyMessage(method, args);
		transport.sendMessage(msg);
	}

	void closeSession() {
		transport.close();
		synchronized(reqtable) {
			for(Map.Entry<Integer,Future> pair : reqtable.entrySet()) {
				// FIXME error result
				Future f = pair.getValue();
				f.setResult(null,org.msgpack.object.RawType.create("session closed"));
			}
			reqtable.clear();
		}
	}

	public void transportConnectFailed() {  // FIXME error rseult
	//	synchronized(reqtable) {
	//		for(Map.Entry<Integer,Future> pair : reqtable.entrySet()) {
	//			// FIXME
	//			Future f = pair.getValue();
	//			f.setResult(null,null);
	//		}
	//		reqtable.clear();
	//	}
	}

	public void onResponse(int msgid, MessagePackObject result, MessagePackObject error) {
		Future f;
		synchronized(reqtable) {
			f = reqtable.remove(msgid);
		}
		if(f == null) {
			// FIXME log
			return;
		}
		f.setResult(result, error);
	}

	void stepTimeout() {
		List<Future> timedout = new ArrayList<Future>();
		synchronized(reqtable) {
			for(Iterator<Map.Entry<Integer,Future>> it = reqtable.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry<Integer,Future> pair = it.next();
				Future f = pair.getValue();
				if(f.stepTimeout()) {
					it.remove();
					timedout.add(f);
				}
			}
		}
		for(Future f : timedout) {
			// FIXME error result
			f.setResult(null,org.msgpack.object.RawType.create("timedout"));
		}
	}
}

