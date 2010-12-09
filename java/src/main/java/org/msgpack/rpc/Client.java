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

import java.io.Closeable;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.rpc.address.Address;
import org.msgpack.rpc.address.IPAddress;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.config.TcpClientConfig;

public class Client extends Session implements Closeable {
	private ScheduledFuture<?> timer;

	public Client(String host, int port) throws UnknownHostException {
		this(new IPAddress(host, port), new TcpClientConfig(), EventLoop.defaultEventLoop());
	}

	public Client(String host, int port, ClientConfig config) throws UnknownHostException {
		this(new IPAddress(host, port), config, EventLoop.defaultEventLoop());
	}

	public Client(String host, int port, EventLoop loop) throws UnknownHostException {
		this(new IPAddress(host, port), new TcpClientConfig(), loop);
	}

	public Client(String host, int port, ClientConfig config, EventLoop loop) throws UnknownHostException {
		this(new IPAddress(host, port), config, loop);
	}

	public Client(InetSocketAddress address) {
		this(new IPAddress(address), new TcpClientConfig(), EventLoop.defaultEventLoop());
	}

	public Client(InetSocketAddress address, ClientConfig config) {
		this(new IPAddress(address), config, EventLoop.defaultEventLoop());
	}

	public Client(InetSocketAddress address, EventLoop loop) {
		this(new IPAddress(address), new TcpClientConfig(), loop);
	}

	public Client(InetSocketAddress address, ClientConfig config, EventLoop loop) {
		this(new IPAddress(address), config, loop);
	}

	Client(Address address, ClientConfig config, EventLoop loop) {
		super(address, config, loop);
		startTimer();
	}

	private void startTimer() {
		Runnable command = new Runnable() {
			public void run() {
				stepTimeout();
			}
		};
		timer = loop.getScheduledExecutor().scheduleAtFixedRate(command, 1000, 1000, TimeUnit.MILLISECONDS);
	}

	public void close() {
		timer.cancel(false);
		closeSession();
	}
}

