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

import java.io.*;
import java.util.*;
import java.net.*;
import org.msgpack.rpc.transport.*;
import org.msgpack.*;

public class Server extends SessionPool {
	private Dispatcher dp;
	private ServerTransport stran;

	public Server() {
		super();
	}

	public Server(ClientTransport transport) {
		super(transport);
	}

	public Server(EventLoop loop) {
		super(loop);
	}

	public Server(ClientTransport transport, EventLoop loop) {
		super(transport, loop);
	}

	public void serve(Dispatcher dp) {
		this.dp = dp;
	}

	public void listen(String host, int port) throws UnknownHostException, IOException {
		listen(new TCPServerTransport(new IPAddress(host,port)));
	}

	public void listen(int port) throws IOException {
		listen(new TCPServerTransport(new IPAddress(port)));
	}

	public void listen(ServerTransport stran) throws IOException {
		stran.listen(this);
		this.stran = stran;
	}

	public void close() {
		stran.close();
		super.close();
	}

	// FIXME package local scope
	public void onRequest(MessageSendable ms, int msgid,
			String method, MessagePackObject[] args) {
		dp.dispatch(new Request(ms, msgid, method, args));
	}

	// FIXME package local scope
	public void onNotify(String method, MessagePackObject[] args) {
		dp.dispatch(new Request(method, args));
	}
}

