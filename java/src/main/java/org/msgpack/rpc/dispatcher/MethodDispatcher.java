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
package org.msgpack.rpc.dispatcher;

import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.lang.reflect.*;
import org.msgpack.rpc.reflect.Invoker;
import org.msgpack.rpc.reflect.Reflect;
import org.msgpack.rpc.reflect.MethodSelector;
import org.msgpack.rpc.*;

public class MethodDispatcher implements Dispatcher {
	protected Map<String, Invoker> methodMap;
	protected Object target;

	public MethodDispatcher(Object target) {
		this(target, target.getClass());
	}

	// FIXME List<DispatchOption>
	public MethodDispatcher(Object target, Class<?> iface) {
		// FIXME check target instanceof iface
		this(target, MethodSelector.selectRpcServerMethod(iface));
	}

	public MethodDispatcher(Object target, Method[] methods) {
		// FIXME check target instanceof method.getClass()
		this.target = target;
		this.methodMap = new HashMap<String, Invoker>();
		for(Method method : methods) {
			// FIXME check duplication of the names
		    methodMap.put(method.getName(), Reflect.reflectInvoker(method));
		}
	}

	public void dispatch(Request request) throws Exception {
		Invoker ivk = methodMap.get(request.getMethodName());
		if(ivk == null) {
			// FIXME
			throw new IOException(".CallError.NoMethodError");
		}
		ivk.invoke(target, request);
	}
}

