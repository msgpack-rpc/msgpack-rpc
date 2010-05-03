package org.msgpack.rpc.client;

import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.msgpack.rpc.client.netty.RPCClientPipelineFactory;

public class EventLoop {
	protected ChannelFactory factory;
	protected ClientBootstrap bootstrap;
	
	public EventLoop() {
        this.factory = new NioClientSocketChannelFactory(
        		Executors.newCachedThreadPool(),
                Executors.newCachedThreadPool());
        this.bootstrap = new ClientBootstrap(factory);
	}
	
	public synchronized void setPipelineFactory(ChannelPipelineFactory factory) {
		this.bootstrap.setPipelineFactory(factory);
	}
	
	public synchronized ChannelFuture connect(Address addr) {
		return bootstrap.connect(new InetSocketAddress(addr.getHost(), addr.getPort()));
	}
	
	public synchronized void shutdown() {
		bootstrap.releaseExternalResources();
	}
}
