package org.msgpack.rpc.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ConnectionlessBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.socket.DatagramChannelFactory;
import org.jboss.netty.channel.socket.nio.NioDatagramChannelFactory;

public class UDPServer extends Server {
    protected final InetSocketAddress addr;
    protected final ConnectionlessBootstrap bootstrap;
    protected final DatagramChannelFactory factory;
    protected Channel ch;

    public UDPServer(String host, int port, Object userHandler) {
        this(new InetSocketAddress(host, port), userHandler);
    }

    public UDPServer(InetSocketAddress addr, Object userHandler) {
        this.addr = addr;
        this.factory = new NioDatagramChannelFactory(Executors.newCachedThreadPool());
        bootstrap = new ConnectionlessBootstrap(factory);
        bootstrap.setOption("receiveBufferSize", 65536);
        bootstrap.setOption("broadcast", "false");
        bootstrap.setPipelineFactory(new RPCServerPipelineFactory(userHandler, false));

    }

    @Override
    public synchronized void serv() throws IOException {
        ch = bootstrap.bind(addr);
    }
    
    @Override
    public synchronized void stop() {
        if (ch != null)
            ch.close().awaitUninterruptibly();
        factory.releaseExternalResources();
    }
}
