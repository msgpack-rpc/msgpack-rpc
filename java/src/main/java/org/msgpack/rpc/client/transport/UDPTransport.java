package org.msgpack.rpc.client.transport;

import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.Session;

public class UDPTransport extends Transport {
    protected UDPSocket socket;

    public UDPTransport(Session session, EventLoop loop) {
        super(session, loop);
        this.socket = new UDPSocket(session.getAddress(), loop, this);
    }

    /**
     * Send the message to the remote server.
     *
     * This method tries to hide the connection latency.
     * If it's already connected, then the message is sent immediately.
     * Otherwise, the message is temporarily buffered in the
     * this.pendingMessages. Once it's connected, the buffered messages are
     * sent to the server.
     * 
     * @param msg the message to send.
     * @throws Exception
     */
    @Override
    public synchronized void sendMessage(Object msg) throws Exception {
        socket.trySend(msg);
    }

    /**
     * Close the connection associated with this transport.
     */
    @Override
    public synchronized void tryClose() {
        socket.tryClose();
    }
    
    /**
     * The callback function, called when the connection is established.
     * @throws Exception
     */
    public void onConnected() throws Exception {
        throw new Exception("shoud not be called");
    }
    
    /**
     * The callback called when the message arrives
     * @param replyObject the received object, already unpacked.
     * @throws Exception
     */
    public void onMessageReceived(Object replyObject) throws Exception {
        session.onMessageReceived(replyObject);
    }

    /**
     * The callback function, called when the connection failed.
     */
    public void onConnectFailed() throws Exception {
        throw new Exception("shoud not be called");
    }
    
    /**
     * The callback called when the connection closed.
     */
    public void onClosed() {
        tryClose();
        session.onClosed();
    }
    
    /**
     * The callback called when the error occurred.
     * @param e occurred exception.
     */
    public void onFailed(Exception e) {
        tryClose();
        session.onFailed(e);
    }
}
