package org.msgpack.rpc.client;

import java.net.InetSocketAddress;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.msgpack.rpc.client.netty.RPCClientPipelineFactory;

public class EventLoop {
    protected ExecutorService bossService;
    protected ExecutorService workerService;
    protected ClientBootstrap bootstrap;
    
    public EventLoop() {
        // using same ExecutorService across same EventLoop
        bossService = Executors.newCachedThreadPool();
        workerService = Executors.newCachedThreadPool();
    }
    
    public ClientBootstrap createBootstrap() {
        ChannelFactory factory = new NioClientSocketChannelFactory(bossService, workerService);
        return new ClientBootstrap(factory);
    }
    
    public synchronized void shutdown() {
        bossService.shutdown();
        workerService.shutdown();
    }
}
