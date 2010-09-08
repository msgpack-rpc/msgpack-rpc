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
import java.net.*;
import java.util.*;
import java.util.concurrent.*;
import org.msgpack.rpc.transport.*;

public class Client extends Session implements Closeable {
	public Client(String host, int port) throws UnknownHostException {
		this(new TCPClientTransport(), new IPAddress(host, port), EventLoop.defaultEventLoop());
	}

	public Client(ClientTransport transport, InetSocketAddress address) {
		this(transport, address, EventLoop.defaultEventLoop());
	}

	public Client(ClientTransport transport, InetSocketAddress address, EventLoop loop) {
		this(transport, new IPAddress(address), loop);
	}

	private Client(ClientTransport transport, Address address, EventLoop loop) {
		super(transport, address, loop);
		startTimer();
	}

	private void startTimer() {
		Runnable command = new Runnable() {
			public void run() {
				stepTimeout();
				// FIXME 終わるタイミング step if closed
			}
		};
		loop.getExecutor().scheduleAtFixedRate(command, 1000, 1000, TimeUnit.MILLISECONDS);
	}

	public void close() {
		closeSession();
	}

	// FIXME EventLoopHolder interface
	public EventLoop getEventLoop() {
		return loop;
	}
}

