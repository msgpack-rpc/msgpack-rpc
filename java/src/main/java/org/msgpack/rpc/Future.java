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

import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.msgpack.MessagePackObject;
import org.msgpack.Template;
import org.msgpack.template.TemplateRegistry;
import org.msgpack.rpc.error.*;

public class Future<V> implements java.util.concurrent.Future<V> {
	private FutureImpl impl;
	private Template resultTemplate;

	Future(FutureImpl impl) {
		this.impl = impl;
		this.resultTemplate = null;
	}

	Future(FutureImpl impl, Template resultTemplate) {
		this.impl = impl;
		this.resultTemplate = resultTemplate;
	}

	public Future(Future<MessagePackObject> future, Class<V> resultClass) {
		this.impl = future.impl;
		if(resultClass != void.class) {
			this.resultTemplate = TemplateRegistry.lookup(resultClass);
		}
	}

	public Future(Future<MessagePackObject> future, Template resultTemplate) {
		this.impl = future.impl;
		this.resultTemplate = resultTemplate;
	}

	public void attachCallback(Runnable callback) {
		impl.attachCallback(callback);
	}

	public V get() throws InterruptedException {
		join();
		checkThrowError();
		return getResult();
	}

	public V get(long timeout, TimeUnit unit) throws InterruptedException, TimeoutException {
		join(timeout, unit);
		checkThrowError();
		return getResult();
	}

	public void join() throws InterruptedException {
		impl.join();
	}

	public void join(long timeout, TimeUnit unit) throws InterruptedException, TimeoutException {
		impl.join(timeout, unit);
	}

	public boolean isDone() {
		return impl.isDone();
	}

	public boolean cancel(boolean mayInterruptIfRunning) {
		// FIXME
		return false;
	}

	public boolean isCancelled() {
		// FIXME
		return false;
	}

	public V getResult() {
		MessagePackObject result = impl.getResult();
		if(resultTemplate == null) {
			return (V)result;
		} else {
			return (V)result.convert(resultTemplate);
		}
	}

	public MessagePackObject getError() {
		return impl.getError();
	}

	private void checkThrowError() {
		if(!getError().isNil()) {
			// FIXME exception
			throw new RemoteError(getError());
		}
	}
}

