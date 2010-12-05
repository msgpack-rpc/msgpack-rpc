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
package org.msgpack.rpc.reflect;

import java.lang.reflect.*;
import org.msgpack.rpc.Future;

public abstract class CallerBuilder {
	public static class MethodEntry {
		private Method method;
		private String rpcName;
		private Type genericReturnType;
		private boolean async;
		private InvokerBuilder.ArgumentEntry[] argumentEntries;

		public MethodEntry(Method method, String rpcName,
				Type genericReturnType, boolean async,
				InvokerBuilder.ArgumentEntry[] argumentEntries) {
			this.method = method;
			this.rpcName = rpcName;
			this.genericReturnType = genericReturnType;
			this.async = async;
			this.argumentEntries = argumentEntries;
		}

		public Method getMethod() {
			return method;
		}

		public String getRpcName() {
			return rpcName;
		}

		public Type getGenericReturnType() {
			return genericReturnType;
		}

		public boolean isReturnTypeVoid() {
			return genericReturnType == void.class;
		}

		public boolean isAsync() {
			return async;
		}

		public InvokerBuilder.ArgumentEntry[] getArgumentEntries() {
			return argumentEntries;
		}
	}

	// Override this method
	public abstract <T> Caller<T> buildCaller(Class<T> iface, MethodEntry[] entries);

	public <T> Caller<T> buildCaller(Class<T> iface) {
		checkValidation(iface);
		MethodEntry[] entries = readMethodEntries(iface);
		return buildCaller(iface, entries);
	}

	private static CallerBuilder instance;

	synchronized private static CallerBuilder getInstance() {
		if(instance == null) {
			instance = selectDefaultCallerBuilder();
		}
		return instance;
	}


	private static CallerBuilder selectDefaultCallerBuilder() {
		// TODO
		//try {
		//	// FIXME JavassistCallerBuilder doesn't work on DalvikVM
		//	if(System.getProperty("java.vm.name").equals("Dalvik")) {
		//		return ReflectionCallerBuilder.getInstance();
		//	}
		//} catch (Exception e) {
		//}
		//return JavassistCallerBuilder.getInstance();
		return ReflectionCallerBuilder.getInstance();
	}

	synchronized static void setInstance(CallerBuilder builder) {
		instance = builder;
	}

	public static <T> Caller<T> build(Class<T> iface) {
		return getInstance().buildCaller(iface);
	}


	static boolean isAsyncMethod(Method targetMethod) {
		// FIXME return type instanceof Future
		return false;
	}


	private static void checkValidation(Class<?> iface) {
		if(!iface.isInterface()) {
			throw new IllegalArgumentException("not interface: "+iface);
		}
		// TODO
	}

	static MethodEntry[] readMethodEntries(Class<?> iface) {
		Method[] methods = MethodSelector.selectRpcClientMethod(iface);

		MethodEntry[] result = new MethodEntry[methods.length];
		for(int i=0; i < methods.length; i++) {
			Method method = methods[i];

			InvokerBuilder.ArgumentEntry[] argumentEntries =
				InvokerBuilder.readArgumentEntries(method, false);

			boolean async = isAsyncMethod(method);

			String rpcName = method.getName();
			if(async) {
				// TODO: modify name? e.g. remove /Async$/
			}

			Class<?> returnType = (Class<?>)method.getGenericReturnType();
			if(async) {
				// TODO: actual return type is Future<HERE>
			}

			result[i] = new MethodEntry(method, rpcName,
					returnType, async, argumentEntries);
		}

		return result;
	}
}

