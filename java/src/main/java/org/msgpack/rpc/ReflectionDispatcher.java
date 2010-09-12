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
import java.lang.reflect.*;
import java.util.*;
import org.msgpack.*;

public class ReflectionDispatcher implements Dispatcher {
	private Object target;
	protected Map<String, Invoker> methodMap = new HashMap<String, Invoker>();

	public ReflectionDispatcher(Object target) {
		this.target = target;
		Method[] methods = target.getClass().getMethods();
		for (Method method : methods) {
			Invoker ivk = createInvoker(method);
			if(ivk != null) {
				methodMap.put(method.getName(), ivk);
			}
		}
	}

	private Invoker createInvoker(Method method) {
		if(!method.isAccessible()) {
			return null;
		}

		Class<?>[] types = method.getParameterTypes();

		if(types.length == 0) {
			// FIXME error?
			return null;
		}

		if(types.length == 1 && types[0] == Request.class) {
			return new DirectInvoker(method);
		}

		// FIXME other
		return new MessagePackObjectInvoker(method);
	}

	private interface Invoker {
		void invoke(Request request) throws Exception;
	}

	private class DirectInvoker implements Invoker {
		private Method method;
		public DirectInvoker(Method method) {
			this.method = method;
		}
		public void invoke(Request request) throws Exception {
			method.invoke(target, request);
		}
	}

	private class MessagePackObjectInvoker implements Invoker {
		private Method method;
		private int num;
		public MessagePackObjectInvoker(Method method) {
			this.method = method;
			this.num = method.getParameterTypes().length;
		}
		public void invoke(Request request) throws Exception {
			MessagePackObject[] args = request.getArguments();
			if(args.length < num) {
				throw new IOException(".CallError.ArgumentError");
			}
			if(args.length > num) {
				MessagePackObject[] args2 = new MessagePackObject[num];
				System.arraycopy(args, 0, args2, 0, num);
				args = args2;
			}
			method.invoke(target, args);
		}
	}

	public void dispatch(Request request) throws Exception {
		Invoker ivk = methodMap.get(request.getMethodName());
		if(ivk == null) {
			// FIXME
			throw new IOException(".CallError.NoMethodError");
		}
		ivk.invoke(request);
	}
}

