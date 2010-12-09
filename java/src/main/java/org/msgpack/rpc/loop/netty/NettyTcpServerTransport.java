//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
package org.msgpack.rpc.loop.netty;

import org.jboss.netty.channel.Channel;
import org.jboss.netty.bootstrap.ServerBootstrap;
import org.msgpack.rpc.Server;
import org.msgpack.rpc.config.TcpServerConfig;
import org.msgpack.rpc.transport.RpcMessageHandler;
import org.msgpack.rpc.transport.ServerTransport;
import org.msgpack.rpc.address.Address;

class NettyTcpServerTransport implements ServerTransport {
	private Server server;
	private Channel listenChannel;

	NettyTcpServerTransport(TcpServerConfig config, Server server,
			NettyEventLoop loop) {
		// TODO: check server != null
		this.server = server;

		Address address = config.getListenAddress();
		RpcMessageHandler handler = new RpcMessageHandler(server);
		handler.useThread(true);

		ServerBootstrap bootstrap = new ServerBootstrap(loop.getServerFactory());
		bootstrap.setPipelineFactory(new StreamPipelineFactory(handler));
		bootstrap.setOption("child.tcpNoDelay", true);
		bootstrap.setOption("reuseAddress", true);

		this.listenChannel = bootstrap.bind(address.getSocketAddress());
	}

	public void close() {
		listenChannel.close();
	}
}

