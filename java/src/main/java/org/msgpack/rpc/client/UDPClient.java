package org.msgpack.rpc.client;

import org.msgpack.rpc.client.transport.UDPTransport;
import org.msgpack.rpc.client.transport.Transport;

public class UDPClient extends Client {
    public UDPClient(String host, int port, EventLoop loop) {
        super(host, port, loop);
    }

    /**
     * Create new transport when it's not available. If exists, return that.
     * @return transport class
     */
    @Override
    protected synchronized Transport getTransport() {
        if (transport != null) return transport;
        transport = new UDPTransport(this, loop);
        return transport;
    }
}
