package org.msgpack.rpc.client.transport;

import java.util.ArrayList;
import java.util.List;

import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.Session;

/**
 * TCPTransport sends/receives the data through TCP, by using underlying
 * TCPSocket layer.
 *
 * This class also hides the latency of establishing the connection. If the
 * connection is not established. the sending messages are temporarily queued.
 * Then, they are actually sent to the network when it's connected.
 */
public class TCPTransport extends Transport {
    protected Boolean isConnecting;
    protected Boolean isConnected;
    protected TCPSocket socket;
    protected List<Object> pendingMessages;

    public TCPTransport(Session session, EventLoop loop) {
        super(session, loop);
        this.isConnecting = false;
        this.isConnected = false;
        this.socket = new TCPSocket(session.getAddress(), loop, this);
        this.pendingMessages = new ArrayList<Object>();
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
        if (isConnected) {
            socket.trySend(msg);
        } else {
            if (!isConnecting) {
                socket.tryConnect();
                isConnecting = true;
            }
            pendingMessages.add(msg);
        }
    }

    /**
     * Send the pending messages.
     * @throws Exception
     */
    protected void trySendPending() throws Exception {
        Object[] msgs;
        synchronized(this) {
            msgs = pendingMessages.toArray();
            pendingMessages.clear();
        }
        for (Object msg : msgs)
            socket.trySend(msg);
    }

    /**
     * Close the connection associated with this transport.
     */
    @Override
    public synchronized void tryClose() {
        if (socket != null)
            socket.tryClose();
        isConnecting = false;
        isConnected = false;
        pendingMessages.clear();
    }
    
    /**
     * The callback function, called when the connection is established.
     * @throws Exception
     */
    public synchronized void onConnected() throws Exception {
        isConnecting = false;
        isConnected = true;
        trySendPending();
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
    public void onConnectFailed() {
        tryClose();
        session.onConnectFailed();
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
