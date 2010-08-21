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

import junit.framework.TestCase;

import org.junit.Test;
import org.msgpack.rpc.client.Client;
import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.TCPClient;
import org.msgpack.rpc.client.UDPClient;
import org.msgpack.rpc.server.TCPServer;
import org.msgpack.rpc.server.UDPServer;

public class ReconnectionTest extends TestCase {
    static final int NUM_ELOOP = 3;
    static final int NUM_RECON = 50;
    
    @Test    
    public void testTCP() throws Exception {
        ServerMock sm = new ServerMock(new TCPServer("0.0.0.0", 19850, this));
        sm.startServer();
        try {
            for (int i = 0; i < NUM_ELOOP; i++) {
                EventLoop loop = new EventLoop();
                try {
                    Client c = new TCPClient("localhost", 19850, loop);
                    testRPCReconnection(c);
                } finally {
                    loop.shutdown();
                }
            }
        } finally {
            sm.stopServer();
        }
    }
    
    @Test
    public void testUDP() throws Exception {
        ServerMock sm = new ServerMock(new UDPServer("0.0.0.0", 19850, this));
        sm.startServer();
        try {
            for (int i = 0; i < NUM_ELOOP; i++) {
                EventLoop loop = new EventLoop();
                try {
                    Client c = new UDPClient("localhost", 19850, loop);
                    testRPCReconnection(c);
                } finally {
                    loop.shutdown();
                }
            }
        } finally {
            sm.stopServer();
        }
    }
    
    public int intFunc1(int a) { return a; }

    protected void testRPCReconnection(Client c) throws Exception {
        for (int i = 0; i < NUM_RECON; i++) {
            // call RPC once, reconnection happens
            Object o;
            o = c.call("intFunc1", i);
            assertEquals(i, ((Number)o).intValue());
            // close the client
            c.close();
        }
    }
}
