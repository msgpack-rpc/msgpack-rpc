//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 Kazuki Ohta
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
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
