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
package org.msgpack.rpc.loop;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.msgpack.rpc.Session;
import org.msgpack.rpc.Server;
import org.msgpack.rpc.transport.ClientTransport;
import org.msgpack.rpc.transport.ServerTransport;
import org.msgpack.rpc.config.ClientConfig;
import org.msgpack.rpc.config.ServerConfig;
import org.msgpack.rpc.config.TcpClientConfig;
import org.msgpack.rpc.config.TcpServerConfig;
import org.msgpack.rpc.loop.netty.NettyEventLoopFactory;

public abstract class EventLoop {
	static private EventLoopFactory loopFactory;
	static private EventLoop defaultEventLoop;

	static public synchronized void setFactory(EventLoopFactory factory) {
		loopFactory = factory;
	}

	static private synchronized EventLoopFactory getFactory() {
		if(loopFactory == null) {
			loopFactory = new NettyEventLoopFactory();
		}
		return loopFactory;
	}

	static public synchronized void setDefaultEventLoop(EventLoop eventLoop) {
		defaultEventLoop = eventLoop;
	}

	static public synchronized EventLoop defaultEventLoop() {
		if(defaultEventLoop == null) {
			defaultEventLoop = start();
		}
		return defaultEventLoop;
	}


	static public EventLoop start() {
		return start(
				Executors.newCachedThreadPool());
	}

	static public EventLoop start(
			ExecutorService workerExecutor) {
		return start(
				workerExecutor,
				Executors.newCachedThreadPool());
	}

	static public EventLoop start(
			ExecutorService workerExecutor,
			ExecutorService ioExecutor) {
		return start(
				workerExecutor,
				ioExecutor,
				Executors.newScheduledThreadPool(2));
	}

	static public EventLoop start(
			ExecutorService workerExecutor,
			ExecutorService ioExecutor,
			ScheduledExecutorService scheduledExecutor) {
		return getFactory().make(
				workerExecutor,
				ioExecutor,
				scheduledExecutor);
	}


	private ExecutorService workerExecutor;
	private ExecutorService ioExecutor;
	private ScheduledExecutorService scheduledExecutor;

	public EventLoop(
			ExecutorService workerExecutor,
			ExecutorService ioExecutor,
			ScheduledExecutorService scheduledExecutor) {
		this.workerExecutor = workerExecutor;
		this.scheduledExecutor = scheduledExecutor;
		this.ioExecutor = ioExecutor;
	}

	public ExecutorService getWorkerExecutor() {
		return workerExecutor;
	}

	public ExecutorService getIoExecutor() {
		return ioExecutor;
	}

	public ScheduledExecutorService getScheduledExecutor() {
		return scheduledExecutor;
	}

	public void shutdown() {
		scheduledExecutor.shutdown();
		ioExecutor.shutdown();
		workerExecutor.shutdown();
	}

	public void join() throws InterruptedException {
		// FIXME?
		scheduledExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
		ioExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
		workerExecutor.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
	}

	// FIXME join? close?

	public ClientTransport openTransport(ClientConfig config, Session session) {
		if(config instanceof TcpClientConfig) {
			return openTcpTransport((TcpClientConfig)config, session);
		}
		// FIXME exception
		throw new RuntimeException("Unknown transport config: "+config.getClass().getName());
	}

	public ServerTransport listenTransport(ServerConfig config, Server server) throws IOException {
		if(config instanceof TcpServerConfig) {
			return listenTcpTransport((TcpServerConfig)config, server);
		}
		// FIXME exception
		throw new RuntimeException("Unknown transport config: "+config.getClass().getName());
	}

	protected abstract ClientTransport openTcpTransport(TcpClientConfig config, Session session);

	protected abstract ServerTransport listenTcpTransport(TcpServerConfig config, Server server);
}

