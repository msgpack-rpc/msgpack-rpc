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

public class TCPSocket {
	protected Address address;
	protected EventLoop loop;
	protected TCPTransport transport;

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
	
	public synchronized void tryConnect() throws Exception {
		if (connectFuture != null)
			throw new IOException("already connected");
		connectFuture = bootstrap.connect(new InetSocketAddress(address.getHost(), address.getPort()));
	}
	
	public synchronized void trySend(Object msg) throws Exception {
		if (connectFuture == null || channel == null)
			throw new IOException("not connected, but try send");
		channel.write(msg);
	}
	
	public synchronized void tryClose() {
		if (channel != null && channel.isOpen())
			channel.close().awaitUninterruptibly();
		address = null;
		transport = null;
		connectFuture = null;
		channel = null;
	}

	// callback
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

	// callback
	public synchronized void onConnectFailed() {
		if (transport != null)
			transport.onConnectFailed();
		tryClose();
	}

	// callback
	public synchronized void onMessageReceived(Object replyObject) throws Exception {
		if (transport != null)
			transport.onMessageReceived(replyObject);
	}
	
	// callback
	public synchronized void onClosed() {
		if (transport != null)
			transport.onClosed();
		tryClose();
	}
	
	// callback
	public synchronized void onFailed() {
		if (transport != null)
			transport.onFailed();
		tryClose();
	}
}
