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
import org.jboss.xnio.*;

public class EventLoop {
	private Xnio xnio;
	private ScheduledExecutorService workerExecutor;

	public EventLoop() {
		this(Executors.newScheduledThreadPool(
					Runtime.getRuntime().availableProcessors()*2));
	}

	public EventLoop(ScheduledExecutorService workerExecutor) {
		this(workerExecutor, new XnioConfiguration());
	}

	public EventLoop(ScheduledExecutorService workerExecutor, XnioConfiguration config) {
		if(config.getExecutor() == null) {
			config.setExecutor(workerExecutor);
		}
		try {
			this.xnio = Xnio.create(config);
		} catch (IOException e) {
			// FIXME
			throw new RuntimeException(e.getMessage());
		}
		this.workerExecutor = workerExecutor;
	}

	public Xnio getXnio() {
		return xnio;
	}

	public ScheduledExecutorService getExecutor() {
		return workerExecutor;
	}

	public void shutdown() {
		workerExecutor.shutdown();
		try {
			xnio.close();
		} catch (IOException e) {
			// FIXME
		}
	}

	static private EventLoop defaultEventLoop;

	static public synchronized void setDefaultEventLoop(EventLoop eventLoop) {
		defaultEventLoop = eventLoop;
	}

	static public synchronized EventLoop defaultEventLoop() {
		if(defaultEventLoop == null) {
			defaultEventLoop = new EventLoop();
		}
		return defaultEventLoop;
	}
}

