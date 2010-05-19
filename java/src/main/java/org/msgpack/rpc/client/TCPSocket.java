package org.msgpack.rpc.client;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.msgpack.rpc.client.netty.RPCClientPipelineFactory;

/**
 * TCPSocket establishes the connection, and also sends/receives the object.
 */
public class TCPSocket {
    protected final Address address;
    protected final EventLoop loop;
    protected final TCPTransport transport;

    // netty-specific
    protected ClientBootstrap bootstrap;
    protected ChannelFuture connectFuture;
    protected Channel channel;

    public TCPSocket(Address address, EventLoop loop, TCPTransport transport) {
        this.address = address;
        this.loop = loop;
        this.transport = transport;
        this.connectFuture = null;
        this.channel = null;
        this.bootstrap = loop.createBootstrap();
        this.bootstrap.setPipelineFactory(new RPCClientPipelineFactory(this));
    }
    
    /**
     * Try to connect to the server.
     * @throws Exception
     */
    public synchronized void tryConnect() throws Exception {
        if (connectFuture != null)
            throw new IOException("already connected");
        connectFuture = bootstrap.connect(new InetSocketAddress(address.getHost(), address.getPort()));
    }
    

    /**
     * Try to send the message.
     * @param msg the message to send
     * @throws Exception
     */
    public synchronized void trySend(Object msg) throws Exception {
        if (connectFuture == null || channel == null)
            throw new IOException("not connected, but try send");
        channel.write(msg);
    }

    /**
     * Try to close the connection.
     */
    public synchronized void tryClose() {
        if (channel != null && channel.isOpen())
            channel.close().awaitUninterruptibly();
        connectFuture = null;
        channel = null;
    }

    /**
     * The callback function, called when the connection is established.
     * @throws Exception
     */
    public synchronized void onConnected() throws Exception {
        // connected, but onConnected() called
        if (channel != null)
            throw new IOException("already connected");
        // onConnected() called without tryConnect
        if (connectFuture == null)
            throw new IOException("tryConnect was not called");

        // set channel
        channel = connectFuture.awaitUninterruptibly().getChannel();
        if (connectFuture.isSuccess())
            transport.onConnected();
        else
            onConnectFailed();
    }

    /**
     * The callback function, called when the connection failed.
     */
    public void onConnectFailed() {
        transport.onConnectFailed();
        tryClose();
    }

    /**
     * The callback called when the message arrives
     * @param replyObject the received object, already unpacked.
     * @throws Exception
     */
    public void onMessageReceived(Object replyObject) throws Exception {
        transport.onMessageReceived(replyObject);
    }
    
    /**
     * The callback called when the connection closed.
     */
    public void onClosed() {
        transport.onClosed();
        tryClose();
    }
    
    /**
     * The callback called when the error occurred.
     * @param e occurred exception.
     */
    public synchronized void onFailed(Exception e) {
        transport.onFailed(e);
        tryClose();
    }
}
