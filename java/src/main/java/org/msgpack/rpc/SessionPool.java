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

import java.io.Closeable;
import java.util.Map;
import java.util.HashMap;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.rpc.address.Address;
import org.msgpack.rpc.address.IPAddress;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.config.TcpClientConfig;

public class SessionPool implements Closeable {
    private ClientConfig config;
    private EventLoop loop;
    private Map<Address, Session> pool = new HashMap<Address, Session>();
    private ScheduledFuture<?> timer;

    public SessionPool() {
        this(new TcpClientConfig());
    }

    public SessionPool(ClientConfig config) {
        this(config, EventLoop.defaultEventLoop());
    }

    public SessionPool(EventLoop loop) {
        this(new TcpClientConfig(), loop);
    }

    public SessionPool(ClientConfig config, EventLoop loop) {
        this.config = config;
        this.loop = loop;
        startTimer();
    }

    // FIXME EventLoopHolder interface?
    public EventLoop getEventLoop() {
        return loop;
    }

    public Session getSession(String host, int port)
            throws UnknownHostException {
        return getSession(new IPAddress(host, port));
    }

    public Session getSession(InetSocketAddress address) {
        return getSession(new IPAddress(address));
    }

    Session getSession(Address address) {
        synchronized (pool) {
            Session s = pool.get(address);
            if (s == null) {
                s = new Session(address, config, loop);
                pool.put(address, s);
            }
            return s;
        }
    }

    public void close() {
        timer.cancel(false);
        synchronized (pool) {
            for (Map.Entry<Address, Session> pair : pool.entrySet()) {
                Session s = pair.getValue();
                s.closeSession();
            }
            pool.clear();
        }
    }

    private void startTimer() {
        Runnable command = new Runnable() {
            public void run() {
                stepTimeout();
            }
        };
        timer = loop.getScheduledExecutor().scheduleAtFixedRate(
                command, 1000, 1000, TimeUnit.MILLISECONDS);
    }

    void stepTimeout() {
        synchronized (pool) {
            for (Map.Entry<Address, Session> pair : pool.entrySet()) {
                Session s = pair.getValue();
                s.stepTimeout();
            }
        }
    }
}
