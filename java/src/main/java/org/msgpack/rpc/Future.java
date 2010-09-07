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

import org.msgpack.*;

public class Future {
	private Object lock = new Object();
	private boolean set = false;

	private Session session;
	private MessagePackObject result;
	private MessagePackObject error;
	private int timeout;
	private Runnable callback;

	public Future(Session session) {
		this.session = session;
		this.timeout = session.getTimeout();
	}

	public MessagePackObject get() {
		join();
		return getResult();
	}

	public void join() {
		synchronized(lock) {
			try {
				while(set == false) {
					lock.wait();
				}
			} catch(InterruptedException e) {
				// FIXME
			}
		}
	}

	public void attachCallback(Runnable callback) {
		synchronized(lock) {
			this.callback = callback;
			// FIXME
		}
	}

	public MessagePackObject getResult() {
		return result;
	}

	public MessagePackObject getError() {
		return error;
	}

	public void setResult(MessagePackObject result, MessagePackObject error) {
		synchronized(lock) {
			this.result = result;
			this.error = error;
			this.set = true;
			lock.notifyAll();
			// FIXME callback
		}
	}

	boolean stepTimeout() {
		if(timeout <= 0) {
			return true;
		} else {
			timeout--;
			return false;
		}
	}
}

