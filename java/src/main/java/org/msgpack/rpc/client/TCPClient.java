package org.msgpack.rpc.client;

import org.msgpack.rpc.client.transport.TCPTransport;
import org.msgpack.rpc.client.transport.Transport;

/**
 * The TCPClient class for MessagePack-RPC. If you use this class, the TCP
 * transport is used to send/receive the data. Please lookat Client class
 * for the example codes.
 * 
 * @see Client
 */
public class TCPClient extends Client {
    public TCPClient(String host, int port, EventLoop loop) {
        super(host, port, loop);
    }

    /**
     * Create new transport when it's not available. If exists, return that.
     * @return transport class
     */
    @Override
    protected synchronized Transport getTransport() {
        if (transport != null) return transport;
        transport = new TCPTransport(this, loop);
        return transport;
    }
}
