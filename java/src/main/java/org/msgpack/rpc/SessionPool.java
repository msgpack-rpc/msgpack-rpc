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
import java.util.concurrent.*;
import org.msgpack.rpc.transport.*;
import org.msgpack.*;

public class SessionPool implements Closeable {
	private ClientTransport transport;
	private EventLoop loop;
	private Map<Address, Session> pool = new HashMap<Address, Session>();

	public SessionPool() {
		this(new TCPClientTransport());
	}

	public SessionPool(ClientTransport transport) {
		this(transport, EventLoop.defaultEventLoop());
	}

	public SessionPool(EventLoop loop) {
		this(new TCPClientTransport(), loop);
	}

	public SessionPool(ClientTransport transport, EventLoop loop) {
		this.transport = transport;
		this.loop = loop;
		startTimer();
	}

	// FIXME EventLoopHolder interface
	public EventLoop getEventLoop() {
		return loop;
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

	public Session getSession(Address address) {
		synchronized(pool) {
			Session s = pool.get(address);
			if(s == null) {
				s = new Session(transport, address, loop);
				pool.put(address, s);
			}
			return s;
		}
	}

	public void close() {
		synchronized(pool) {
			for(Map.Entry<Address,Session> pair : pool.entrySet()) {
				Session s = pair.getValue();
				s.closeSession();
			}
			pool.clear();
		}
	}

	void stepTimeout() {
		synchronized(pool) {
			for(Map.Entry<Address,Session> pair : pool.entrySet()) {
				Session s = pair.getValue();
				s.stepTimeout();
			}
		}
	}
}

