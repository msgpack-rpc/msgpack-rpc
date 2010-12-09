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

import org.msgpack.MessagePackObject;
import org.msgpack.Template;
import org.msgpack.template.TemplateRegistry;
import org.msgpack.rpc.error.*;

public class Future<T> {
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

	public Future(Future<MessagePackObject> future, Class<T> resultClass) {
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

	public T get() {
		join();
		if(!getError().isNil()) {
			// FIXME exception
			throw new RemoteError(getError());
		}
		return getResult();
	}

	public void join() {
		impl.join();
	}

	public T getResult() {
		MessagePackObject result = impl.getResult();
		if(resultTemplate == null) {
			return (T)result;
		} else {
			return (T)result.convert(resultTemplate);
		}
	}

	public MessagePackObject getError() {
		return impl.getError();
	}
}

