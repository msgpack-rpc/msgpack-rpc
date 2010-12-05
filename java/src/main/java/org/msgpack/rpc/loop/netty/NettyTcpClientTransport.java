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
import org.jboss.netty.channel.Channels;
import org.jboss.netty.channel.ChannelFuture;
import org.jboss.netty.channel.ChannelFutureListener;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.buffer.ChannelBufferOutputStream;
import org.jboss.netty.buffer.HeapChannelBufferFactory;
import org.jboss.netty.bootstrap.ClientBootstrap;
import org.msgpack.rpc.Session;
import org.msgpack.rpc.config.TcpClientConfig;
import org.msgpack.rpc.transport.RpcMessageHandler;
import org.msgpack.rpc.transport.PooledStreamClientTransport;

class NettyTcpClientTransport extends PooledStreamClientTransport<Channel, ChannelBufferOutputStream> {
	private ClientBootstrap bootstrap;

	NettyTcpClientTransport(TcpClientConfig config, Session session,
			NettyEventLoop loop) {
		// TODO check session.getAddress() instanceof IPAddress
		super(config, session);

		RpcMessageHandler handler = new RpcMessageHandler(session);

		bootstrap = new ClientBootstrap(loop.getClientFactory());
		bootstrap.setPipelineFactory(new StreamPipelineFactory(handler));
		bootstrap.setOption("tcpNoDelay", true);
	}

	private final ChannelFutureListener connectListener =
		new ChannelFutureListener() {
			public void operationComplete(ChannelFuture future) throws Exception {
				if(!future.isSuccess()) {
					onConnectFailed(future.getCause());
					return;
				}
				Channel c = future.getChannel();
				c.getCloseFuture().addListener(closeListener);
				onConnected(c);
			}
		};

	private final ChannelFutureListener closeListener =
		new ChannelFutureListener() {
			public void operationComplete(ChannelFuture future) throws Exception {
				Channel c = future.getChannel();
				onClosed(c);
			}
		};

	@Override
	protected void startConnection() {
		ChannelFuture f = bootstrap.connect(
				session.getAddress().getSocketAddress());
		f.addListener(connectListener);
	}

	@Override
	protected ChannelBufferOutputStream newPendingBuffer() {
		return new ChannelBufferOutputStream(
				ChannelBuffers.dynamicBuffer(
					HeapChannelBufferFactory.getInstance()));
	}

	@Override
	protected void resetPendingBuffer(ChannelBufferOutputStream b) {
		b.buffer().setIndex(0,0);
	}

	@Override
	protected void flushPendingBuffer(ChannelBufferOutputStream b, Channel c) {
		Channels.write(c, b.buffer());
	}

	@Override
	protected void sendMessageChannel(Channel c, Object msg) {
		Channels.write(c, msg);
	}

	@Override
	protected void closeChannel(Channel c) {
		c.close();
	}
}

