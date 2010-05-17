package org.msgpack.rpc.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

@ChannelPipelineCoverage("all")
public class Server {
    protected final InetSocketAddress addr;
    protected final ServerBootstrap bootstrap;
    protected final ChannelFactory factory;
    protected Channel ch;

    public Server(String host, int port, Object userHandler) {
        this(new InetSocketAddress(host, port), userHandler);
    }
    
    public Server(InetSocketAddress addr, Object userHandler) {
        this.addr = addr;
        this.factory = new NioServerSocketChannelFactory(
                Executors.newCachedThreadPool(),
                Executors.newCachedThreadPool());
        bootstrap = new ServerBootstrap(factory);
        bootstrap.setPipelineFactory(new RPCServerPipelineFactory(userHandler));
        bootstrap.setOption("reuseAddress", true);
        bootstrap.setOption("child.tcpNoDelay", true);
        bootstrap.setOption("child.keepAlive", true);
    }

    public synchronized void serv() throws IOException {
        ch = bootstrap.bind(addr);
    }
    
    public synchronized void stop() {
        if (ch != null)
            ch.close().awaitUninterruptibly();
        factory.releaseExternalResources();
    }
}
