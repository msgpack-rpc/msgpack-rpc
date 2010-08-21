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
package org.msgpack.rpc.client;

import org.msgpack.rpc.client.transport.TCPTransport;
import org.msgpack.rpc.client.transport.Transport;

/**
 * The TCPClient class for MessagePack-RPC. If you use this class, the TCP
 * transport is used to send/receive the data. Please lookat Client class
 * for the example codes.
 * 
 * @see Client
 */
public class TCPClient extends Client {
    public TCPClient(String host, int port, EventLoop loop) {
        super(host, port, loop);
    }

    /**
     * Create new transport when it's not available. If exists, return that.
     * @return transport class
     */
    @Override
    protected synchronized Transport getTransport() {
        if (transport != null) return transport;
        transport = new TCPTransport(this, loop);
        return transport;
    }
}
