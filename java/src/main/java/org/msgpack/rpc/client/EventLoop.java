package org.msgpack.rpc.client;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import org.jboss.netty.bootstrap.ClientBootstrap;
import org.jboss.netty.bootstrap.ConnectionlessBootstrap;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.socket.nio.NioClientSocketChannelFactory;
import org.jboss.netty.channel.socket.nio.NioDatagramChannelFactory;

/**
 * I/O loop class used by the Client class.
 * This class wraps the Netty ClientBootstrap and ExecutorService.
 */
public class EventLoop {
    protected final ExecutorService bossService;
    protected final ExecutorService workerService;
    protected final ScheduledExecutorService scheduler;
    
    public EventLoop() {
        bossService = Executors.newCachedThreadPool();
        workerService = Executors.newCachedThreadPool();
        scheduler = Executors.newScheduledThreadPool(1);
    }

    /**
     * Create the ClientBootstrap object.
     * @return the ClientBootstrap object.
     */
    public ClientBootstrap createSocketBootstrap() {
        ChannelFactory factory = new NioClientSocketChannelFactory(bossService, workerService);
        return new ClientBootstrap(factory);
    }
    
    /**
     * Create the ConnectionlessBootstrap object.
     * @return the ConectionlessBootstrap object.
     */
    public ConnectionlessBootstrap createDatagramBootstrap() {
        ChannelFactory factory = new NioDatagramChannelFactory(bossService);
        return new ConnectionlessBootstrap(factory);
    }

    /**
     * Register timer for the fixed interval task.
     * @param intervalSec the interval.
     * @param task the task.
     * @return the ScheduledFuture object, to stop the timer.
     */
    public ScheduledFuture<?> registerTimer(Runnable task, int intervalSec) {
        return scheduler.scheduleAtFixedRate(
            task, 0, intervalSec * 1000, TimeUnit.MILLISECONDS
        );
    }
    
    /**
     * Shutdown the services, launched by ExecutorServices.
     */
    public synchronized void shutdown() {
        bossService.shutdown();
        workerService.shutdown();
        scheduler.shutdown();
    }
}
