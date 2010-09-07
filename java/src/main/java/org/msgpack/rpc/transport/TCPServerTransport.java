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
package org.msgpack.rpc.transport;

import java.io.*;
import java.util.*;
import java.net.*;
import org.msgpack.*;
import org.msgpack.rpc.*;
import org.jboss.xnio.*;
import org.jboss.xnio.channels.*;

public class TCPServerTransport implements ServerTransport {
	private Address address;
	private Server server;
	private TcpServer tcpServer;

	public TCPServerTransport(Address address) {
		this.address = address;
	}

	private final ChannelListener<TcpChannel> openHandler =
		new ChannelListener<TcpChannel>() {
			public void handleEvent(TcpChannel channel) {
			}
		};

	public void listen(Server server) throws IOException {
		this.server = server;
		InetSocketAddress addr = (InetSocketAddress)address.getSocketAddress();
		OptionMap opts = OptionMap.EMPTY;  // FIXME
		this.tcpServer = server.getEventLoop().getXnio().createTcpServer(
				new ChannelListener<TcpChannel>() {
					public void handleEvent(TcpChannel channel) {
						onOpen(channel);
					}
				}, opts);
		this.tcpServer.bind(addr).get();
	}

	public void close() {
		try {
			tcpServer.close();
		} catch(IOException e) {
			// FIXME
		}
	}

	void onOpen(TcpChannel channel) {
		new TCPServerSocket(channel, server);
	}
}

