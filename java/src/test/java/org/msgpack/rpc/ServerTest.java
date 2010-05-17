package org.msgpack.rpc;

import junit.framework.*;
import org.junit.Test;
import org.msgpack.rpc.client.Client;
import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.server.Server;

import static org.junit.Assert.*;

public class ServerTest extends TestCase {
    private Thread serverThread;
    private Server server;

    private void startServer(final Object handler) throws Exception {
        serverThread = new Thread() {
            public void run() {
                try {
                    server = new Server("0.0.0.0", 19850, handler);
                    server.serv();
                } catch (Exception e) {
                    fail();
                }
            }
        };
        serverThread.start();
        Thread.sleep(1000);
    }
    
    private void stopServer() throws Exception {
        server.stop();
        try {
            serverThread.join();
        } catch (InterruptedException e) {}
    }
    
    public int intFunc0() { return 0; }
    public int intFunc1(int a) { return a; }
    public int intFunc2(int a, int b) { return b; }
    
    @Test
    public void testIt() throws Exception {
        startServer(this);
        
        EventLoop loop = new EventLoop();
        try {
            Client c = new Client("localhost", 19850, loop);
            testInt(c);
            c.close();
        } finally {
            loop.shutdown();
        }
        
        stopServer();
    }
    
    protected void testInt(Client c) throws Exception {
        Object o;
        o = c.call("intFunc0");
        assertEquals(0, ((Number)o).intValue());
        o = c.call("intFunc1", 1);
        assertEquals(1, ((Number)o).intValue());
        o = c.call("intFunc2", 1, 2);
        assertEquals(2, ((Number)o).intValue());
    }
}
