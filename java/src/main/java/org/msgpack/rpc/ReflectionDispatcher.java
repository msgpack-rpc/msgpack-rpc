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

public class ReflectionDispatcher implements Dispatcher {
	private Object target;
	protected Map<String, Invoker> methodMap = new HashMap<String, Invoker>();

	public ReflectionDispatcher(Object target) {
		this.target = target;
		Method[] methods = target.getClass().getMethods();
		for (Method method : methods) {
			Invoker ivk;
			ivk = new DirectInvoker(method);
			methodMap.put(method.getName(), ivk);
		}
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

	public void dispatch(Request request) throws Exception {
		Invoker ivk = methodMap.get(request.getMethodName());
		if(ivk == null) {
			// FIXME
			throw new IOException(".CallError.NoMethodError");
		}
		ivk.invoke(request);
	}
}

