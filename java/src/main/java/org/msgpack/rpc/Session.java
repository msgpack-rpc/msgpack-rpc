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
import java.util.concurrent.atomic.AtomicInteger;
import org.msgpack.rpc.transport.*;
import org.msgpack.*;

public class Session {
	protected Address address;
	protected EventLoop loop;
	private int timeout = 3;  // FIXME
	private AtomicInteger seqid = new AtomicInteger(0);
	private MessageSendable transport;
	private Map<Integer, Future> reqtable = new HashMap<Integer, Future>();

	protected Session(ClientTransport transport, Address address, EventLoop loop) {
		this.address = address;
		this.loop = loop;
		// FIXME
		this.transport = transport.createPeer(this);
	}

	public SocketAddress getSocketAddress() {
		return address.getSocketAddress();
	}

	Address getAddress() {
		return address;
	}

	// FIXME package local scope
	public EventLoop getEventLoop() {
		return loop;
	}

	public int getTimeout() {
		return timeout;
	}

	public void setTimeout(int timeout) {
		this.timeout = timeout;
	}

	void closeSession() {
		transport.close();
		synchronized(reqtable) {
			for(Map.Entry<Integer,Future> pair : reqtable.entrySet()) {
				// FIXME
				Future f = pair.getValue();
				f.setResult(null,org.msgpack.object.RawType.create("session closed"));
			}
			reqtable.clear();
		}
	}

	public void transportConnectFailed() {  // FIXME
	//	synchronized(reqtable) {
	//		for(Map.Entry<Integer,Future> pair : reqtable.entrySet()) {
	//			// FIXME
	//			Future f = pair.getValue();
	//			f.setResult(null,null);
	//		}
	//		reqtable.clear();
	//	}
	}

	// FIXME package local scope
	public void onResponse(int msgid, MessagePackObject result, MessagePackObject error) {
		Future f;
		synchronized(reqtable) {
			f = reqtable.remove(msgid);
		}
		if(f == null) {
			// FIXME log
			return;
		}
		f.setResult(result, error);
	}

	public MessagePackObject callApply(String method, Object[] args) {
		return sendRequest(method, args).get();
	}

	public Future callAsyncApply(String method, Object[] args) {
		return sendRequest(method, args);
	}

	public Future sendRequest(String method, Object[] args) {
		int msgid = seqid.getAndAdd(1);
		RequestMessage msg = new RequestMessage(msgid, method, args);
		Future f = new Future(this);

		synchronized(reqtable) {
			reqtable.put(msgid, f);
		}
		transport.sendMessage(msg);

		return f;
	}

	void stepTimeout() {
		List<Future> timedout = new ArrayList<Future>();
		synchronized(reqtable) {
			for(Iterator<Map.Entry<Integer,Future>> it = reqtable.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry<Integer,Future> pair = it.next();
				Future f = pair.getValue();
				if(f.stepTimeout()) {
					it.remove();
					timedout.add(f);
				}
			}
		}
		for(Future f : timedout) {
			// FIXME
			f.setResult(null,org.msgpack.object.RawType.create("timedout"));
		}
	}
}

