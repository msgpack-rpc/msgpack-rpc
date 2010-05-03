package org.msgpack.rpc.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

@ChannelPipelineCoverage("all")
public class Server {
	public Server() {
	}

	public void listen(int port, Object userHandler) throws IOException {
        ChannelFactory factory = new NioServerSocketChannelFactory(
        		Executors.newCachedThreadPool(),
        		Executors.newCachedThreadPool());
        
        ServerBootstrap bootstrap = new ServerBootstrap(factory);
        bootstrap.setPipelineFactory(new RPCServerPipelineFactory(userHandler));
        bootstrap.setOption("reuseAddress", true);
        bootstrap.setOption("child.tcpNoDelay", true);
        bootstrap.setOption("child.keepAlive", true);
        bootstrap.bind(new InetSocketAddress(port));
    }

}
