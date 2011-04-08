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
package org.msgpack.rpc;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.address.Address;
import org.msgpack.rpc.address.IPAddress;
import org.msgpack.rpc.dispatcher.Dispatcher;
import org.msgpack.rpc.dispatcher.MethodDispatcher;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.config.ServerConfig;
import org.msgpack.rpc.config.TcpServerConfig;
import org.msgpack.rpc.transport.ServerTransport;
import org.msgpack.rpc.transport.MessageSendable;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.rpc.error.RPCError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Server extends SessionPool {
	
	private final static Logger logger = LoggerFactory.getLogger(Server.class);
	
	private Dispatcher dp;
	private ServerTransport stran;

	public Server() {
		super();
	}

	public Server(ClientConfig config) {
		super(config);
	}

	public Server(EventLoop loop) {
		super(loop);
	}

	public Server(ClientConfig config, EventLoop loop) {
		super(config, loop);
	}

	public void serve(Dispatcher dp) {
		this.dp = dp;
	}

	public void serve(Object handler) {
		this.dp = new MethodDispatcher(handler);
	}

	public void listen(String host, int port) throws UnknownHostException, IOException {
		listen(new TcpServerConfig(new IPAddress(host,port)));
	}

	public void listen(InetSocketAddress address) throws IOException {
		listen(new TcpServerConfig(new IPAddress(address)));
	}

	public void listen(int port) throws IOException {
		listen(new TcpServerConfig(new IPAddress(port)));
	}

	public void listen(ServerConfig config) throws IOException {
		stran = getEventLoop().listenTransport(config, this);
	}

	public void close() {
		if(stran != null) {
			stran.close();
		}
		super.close();
	}

	public void onRequest(MessageSendable channel,
			int msgid, String method, MessagePackObject args) {
		Request request = new Request(channel, msgid, method, args);
		try {
			dp.dispatch(request);
		} catch(RPCError e) {
			// FIXME
			request.sendError(e.getCode(), e);
		} catch(Exception e) {
			logger.error("Unexpected error occured while calling " + method,e);
			// FIXME request.sendError("RemoteError", e.getMessage());
			request.sendError(e.getMessage());
		}
	}

	public void onNotify(String method, MessagePackObject args) {
		Request request = new Request(method, args);
		try {
			dp.dispatch(request);
		} catch(Exception e) {
			// FIXME ignore?
		}
	}
}

