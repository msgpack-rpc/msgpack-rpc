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
package org.msgpack.rpc.server;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

import org.jboss.netty.bootstrap.ServerBootstrap;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelFactory;
import org.jboss.netty.channel.socket.nio.NioServerSocketChannelFactory;

public class TCPServer extends Server {
    protected final InetSocketAddress addr;
    protected final ServerBootstrap bootstrap;
    protected final ChannelFactory factory;
    protected Channel ch;

    public TCPServer(String host, int port, Object userHandler) {
        this(new InetSocketAddress(host, port), userHandler);
    }

    public TCPServer(InetSocketAddress addr, Object userHandler) {
        this.addr = addr;
        this.factory = new NioServerSocketChannelFactory(
                Executors.newCachedThreadPool(),
                Executors.newCachedThreadPool());
        bootstrap = new ServerBootstrap(factory);
        bootstrap.setPipelineFactory(new RPCServerPipelineFactory(userHandler, true));
        bootstrap.setOption("reuseAddress", true);
        bootstrap.setOption("child.tcpNoDelay", true);
        bootstrap.setOption("child.keepAlive", true);
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
