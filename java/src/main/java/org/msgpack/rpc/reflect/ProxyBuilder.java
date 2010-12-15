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

public abstract class ProxyBuilder {
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
			return genericReturnType == void.class || genericReturnType == Void.class;
		}

		public boolean isAsync() {
			return async;
		}

		public InvokerBuilder.ArgumentEntry[] getArgumentEntries() {
			return argumentEntries;
		}
	}

	// Override this method
	public abstract <T> Proxy<T> buildProxy(Class<T> iface, MethodEntry[] entries);

	public <T> Proxy<T> buildProxy(Class<T> iface) {
		checkValidation(iface);
		MethodEntry[] entries = readMethodEntries(iface);
		return buildProxy(iface, entries);
	}

	private static ProxyBuilder instance;

	synchronized private static ProxyBuilder getInstance() {
		if(instance == null) {
			instance = selectDefaultProxyBuilder();
		}
		return instance;
	}


	private static ProxyBuilder selectDefaultProxyBuilder() {
		// TODO
		//try {
		//	// FIXME JavassistProxyBuilder doesn't work on DalvikVM
		//	if(System.getProperty("java.vm.name").equals("Dalvik")) {
		//		return ReflectionProxyBuilder.getInstance();
		//	}
		//} catch (Exception e) {
		//}
		//return JavassistProxyBuilder.getInstance();
		return ReflectionProxyBuilder.getInstance();
	}

	synchronized static void setInstance(ProxyBuilder builder) {
		instance = builder;
	}

	public static <T> Proxy<T> build(Class<T> iface) {
		return getInstance().buildProxy(iface);
	}


	static boolean isAsyncMethod(Method targetMethod) {
		// return type is Future<T>
		return targetMethod.getReturnType().equals( org.msgpack.rpc.Future.class ) ||
			   targetMethod.getReturnType().equals( java.util.concurrent.Future.class );
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
				// removes /Async$/
				if(rpcName.endsWith("Async")) {
					rpcName = rpcName.substring(0, rpcName.length()-5);
				}
			}

			Type returnType = method.getGenericReturnType();
			if(async) {
				// actual return type is Future<HERE>
				returnType = ((ParameterizedType)returnType).getActualTypeArguments()[0];
			}

			result[i] = new MethodEntry(method, rpcName,
					returnType, async, argumentEntries);
		}

		return result;
	}
}

