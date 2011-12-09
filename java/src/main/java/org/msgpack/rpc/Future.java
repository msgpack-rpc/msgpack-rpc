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

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import org.msgpack.MessagePack;
import org.msgpack.annotation.Message;
import org.msgpack.type.Value;
import org.msgpack.template.Template;
import org.msgpack.template.TemplateRegistry;
import org.msgpack.rpc.error.*;
import org.msgpack.unpacker.Converter;
import org.msgpack.unpacker.Unpacker;

public class Future<V> implements java.util.concurrent.Future<V> {
	private FutureImpl impl;
	private Template resultTemplate;
    private MessagePack messagePack;

	Future(MessagePack messagePack,FutureImpl impl) {
        this(messagePack,impl,null);
	}

	Future(MessagePack messagePack,FutureImpl impl, Template resultTemplate) {
		this.impl = impl;
		this.resultTemplate = resultTemplate;
        this.messagePack = messagePack;
	}

	public Future(MessagePack messagePack,Future<Value> future, Class<V> resultClass) {
        this(messagePack,future.impl,null);
		if(resultClass != void.class) {
			this.resultTemplate = messagePack.lookup(resultClass);
		}
	}

	public Future(MessagePack messagePack,Future<Value> future, Template resultTemplate) {
        this(messagePack,future.impl,resultTemplate);
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
		Value result = impl.getResult();
		if(resultTemplate == null) {
			return (V)result;
		} else if(result.isNilValue()) {
			return null;
		} else {
            try {
                return (V)resultTemplate.read(new Converter(messagePack,result),null);
                //return (V)messagePack.c(result,);// result.convert(resultTemplate);
            } catch (IOException e) {
                return null;
            }
        }
	}

	public Value getError() {
		return impl.getError();
	}

	private void checkThrowError() {
		if(!getError().isNilValue()) {
			// FIXME exception
			throw new RemoteError(getError());
		}
	}
}

