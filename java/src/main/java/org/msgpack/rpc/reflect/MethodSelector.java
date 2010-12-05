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
import java.util.List;
import java.util.ArrayList;

public class MethodSelector {
	public static Method[] selectRpcServerMethod(Class<?> iface) {
		List<Method> methods = new ArrayList();
		for(Method method : iface.getDeclaredMethods()) {
			if(isRpcServerMethod(method)) {
				methods.add(method);
			}
		}
		return methods.toArray(new Method[0]);
	}

	public static Method[] selectRpcClientMethod(Class<?> iface) {
		List<Method> methods = new ArrayList();
		for(Method method : iface.getDeclaredMethods()) {
			if(isRpcClientMethod(method)) {
				methods.add(method);
			}
		}
		return methods.toArray(new Method[0]);
	}

	public static boolean isRpcServerMethod(Method method) {
		return isRpcMethod(method) &&
			!CallerBuilder.isAsyncMethod(method);
	}

	public static boolean isRpcClientMethod(Method method) {
		return isRpcMethod(method) &&
			!InvokerBuilder.isAsyncMethod(method);
	}

	private static boolean isRpcMethod(Method method) {
		// TODO @Ignore annotation
		// TODO @Direct annotation

		int mod = method.getModifiers();
		if(Modifier.isStatic(mod)) {
			return false;
		}

		if(!Modifier.isPublic(mod)) {
			// TODO @Dispatch annotation
			// TODO default rule
			return false;
		}

		return true;
	}

}

