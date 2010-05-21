package org.msgpack.rpc.client;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;

/**
 * I/O loop class used by the Client class.
 * This class wraps the Netty ClientBootstrap and ExecutorService.
 */
public class EventLoop {
    protected ExecutorService bossService;
    protected ExecutorService workerService;
    protected ClientBootstrap bootstrap;
    
    public EventLoop() {
        // using same ExecutorService across same EventLoop
        bossService = Executors.newCachedThreadPool();
        workerService = Executors.newCachedThreadPool();
    }

    /**
     * Create the ClientBootstrap object.
     * @return the ClientBootstrap object.
     */
    public ClientBootstrap createBootstrap() {
        ChannelFactory factory = new NioClientSocketChannelFactory(bossService, workerService);
        return new ClientBootstrap(factory);
    }

    /**
     * Shutdown the services, launched by ExecutorServices.
     */
    public synchronized void shutdown() {
        bossService.shutdown();
        workerService.shutdown();
    }
}
