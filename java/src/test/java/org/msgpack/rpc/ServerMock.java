package org.msgpack.rpc;

import junit.framework.Assert;

import org.msgpack.rpc.server.Server;

public class ServerMock {
    private Thread serverThread;
    private Server server;
        
    public ServerMock(Server s) {
        this.server = s;
    }

    public void startServer() throws Exception {
        serverThread = new Thread() {
            public void run() {
                try {
                    server.serv();
                } catch (Exception e) {
                    Assert.fail();
                }
            }
        };
        serverThread.start();
        Thread.sleep(1500);
    }
    
    public void stopServer() throws Exception {
        server.stop();
        try {
            serverThread.join();
        } catch (InterruptedException e) {}
    }
}
