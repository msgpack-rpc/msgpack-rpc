package org.msgpack.rpc.client.transport;

import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.Session;

abstract public class Transport {
    protected final Session session;
    protected final EventLoop loop;
    
    public Transport(Session session, EventLoop loop) {
        this.session = session;
        this.loop = loop;
    }

    abstract public void sendMessage(Object msg) throws Exception;
    abstract public void tryClose();
}
