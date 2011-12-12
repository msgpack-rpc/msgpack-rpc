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

import java.io.IOException;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.*;
import org.msgpack.rpc.*;
import org.msgpack.*;
import org.msgpack.rpc.loop.netty.MessagePackEncoder;
import org.msgpack.template.*;
import org.msgpack.type.Value;
import org.msgpack.unpacker.Converter;

public class ReflectionProxyBuilder extends ProxyBuilder {

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

		public Template getReturnTypeTemplate() {
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
				Future<Value> f = s.callAsyncApply(e.getRpcName(), params);
				return new Future<Object>(messagePack, f, e.getReturnTypeTemplate());
			} else {
				Value obj = s.callApply(e.getRpcName(), params);
				if(obj.isNilValue()){
					return null;
				}else{
					Template tmpl = e.getReturnTypeTemplate();
					if(tmpl == null) {
						return null;
					}
                    try {
                        return tmpl.read(new Converter(messagePack,obj),null);
                        //return messagePack.convert(obj,tmpl);// obj.convert(tmpl);
                    } catch (IOException e1) {
                        return null;
                    }
                }
			}
		}
	}

	public class ReflectionProxy<T> implements Proxy<T> {
		private Class<T> iface;
		private Map<Method, ReflectionMethodEntry> entryMap;

		public ReflectionProxy(Class<T> iface, Map<Method, ReflectionMethodEntry> entryMap) {
			this.iface = iface;
			this.entryMap = entryMap;
		}

		public T newProxyInstance(Session s) {
			ReflectionHandler handler = new ReflectionHandler(s, entryMap);
			return (T)java.lang.reflect.Proxy.newProxyInstance(
					iface.getClassLoader(), new Class[] { iface }, handler);
		}
	}

    private MessagePack messagePack;

    public ReflectionProxyBuilder(MessagePack messagePack){

        this.messagePack = messagePack;
    }

	public <T> Proxy<T> buildProxy(Class<T> iface, MethodEntry[] entries) {
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
				tmpl = messagePack.lookup(e.getGenericReturnType());// TemplateRegistry.lookup(e.getGenericReturnType(), true);
			}
			entryMap.put(e.getMethod(), new ReflectionMethodEntry(e, tmpl));
		}

		return new ReflectionProxy<T>(iface, entryMap);
	}
}

