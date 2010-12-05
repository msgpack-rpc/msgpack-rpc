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

import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.*;
import org.msgpack.rpc.*;
import org.msgpack.*;
import org.msgpack.template.*;

public class ReflectionCallerBuilder extends CallerBuilder {
	private static ReflectionCallerBuilder instance;
	public synchronized static ReflectionCallerBuilder getInstance() {
		if(instance == null) {
			instance = new ReflectionCallerBuilder();
		}
		return instance;
	}

	private static class ReflectionMethodEntry {
		private String rpcName;
		private Template returnTypeTemplate;
		private boolean async;
		private InvokerBuilder.ArgumentEntry[] argumentEntries;

		public ReflectionMethodEntry(MethodEntry e, Template returnTypeTemplate) {
			this.rpcName = e.getRpcName();
			this.returnTypeTemplate = returnTypeTemplate;
			this.async = e.isAsync();
			this.argumentEntries = e.getArgumentEntries();
		}

		public String getRpcName() {
			return rpcName;
		}

		public Template getreturnTypeTemplate() {
			return returnTypeTemplate;
		}

		public boolean isAsync() {
			return async;
		}

		public Object[] sort(Object[] args) {
			Object[] params = new Object[argumentEntries.length];

			for(int i=0; i < argumentEntries.length; i++) {
				InvokerBuilder.ArgumentEntry e = argumentEntries[i];
				if(!e.isAvailable()) {
					continue;
				}
				if(params.length < e.getIndex()) {
					// FIXME
				}
				if(e.isRequired() && args[i] == null) {
					// TODO type error
				}
				params[i] = args[e.getIndex()];
			}

			return params;
		}
	}

	public class ReflectionHandler implements InvocationHandler {
		private Session s;
		private Map<Method, ReflectionMethodEntry> entryMap;

		public ReflectionHandler(Session s, Map<Method, ReflectionMethodEntry> entryMap) {
			this.s = s;
			this.entryMap = entryMap;
		}

		public Object invoke(Object proxy, Method method, Object[] args) {
			ReflectionMethodEntry e = entryMap.get(method);
			if(e == null) {
				// FIXME
			}
			Object[] params = e.sort(args);
			if(e.isAsync()) {
				// TODO async
				throw new RuntimeException("async is not supported yet");
			} else {
				MessagePackObject obj = s.callApply(e.getRpcName(), params);
				Template tmpl = e.getreturnTypeTemplate();
				if(tmpl == null) {
					return null;
				}
				return obj.convert(tmpl);
			}
		}
	}

	public class ReflectionCaller<T> implements Caller<T> {
		private Class<T> iface;
		private Map<Method, ReflectionMethodEntry> entryMap;

		public ReflectionCaller(Class<T> iface, Map<Method, ReflectionMethodEntry> entryMap) {
			this.iface = iface;
			this.entryMap = entryMap;
		}

		public T newProxyInstance(Session s) {
			ReflectionHandler handler = new ReflectionHandler(s, entryMap);
			return (T)Proxy.newProxyInstance(
					iface.getClassLoader(), new Class[] { iface }, handler);
		}
	}

	public <T> Caller<T> buildCaller(Class<T> iface, MethodEntry[] entries) {
		for(MethodEntry e : entries) {
			Method method = e.getMethod();
			int mod = method.getModifiers();
			if(!Modifier.isPublic(mod)) {
				method.setAccessible(true);
			}
		}

		Map<Method, ReflectionMethodEntry> entryMap = new HashMap<Method, ReflectionMethodEntry>();
		for(int i=0; i < entries.length; i++) {
			MethodEntry e = entries[i];
			Template tmpl;
			if(e.isReturnTypeVoid()) {
				tmpl = null;
			} else {
				tmpl = TemplateRegistry.lookup(e.getGenericReturnType(), true);
			}
			entryMap.put(e.getMethod(), new ReflectionMethodEntry(e, tmpl));
		}

		return new ReflectionCaller<T>(iface, entryMap);
	}
}

