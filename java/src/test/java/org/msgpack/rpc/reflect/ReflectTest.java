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

import org.msgpack.rpc.*;
import org.msgpack.rpc.dispatcher.*;
import org.msgpack.rpc.loop.*;

import java.lang.reflect.Method;
import java.util.*;

import junit.framework.*;

public abstract class ReflectTest extends TestCase {
	static String stringify1(Iterable<String> a) {
		StringBuilder sb = new StringBuilder();
		for(String s : a) {
			sb.append(s);
		}
		return sb.toString();
	}

	static String stringify2(Iterable<? extends Iterable<String>> a) {
		StringBuilder sb = new StringBuilder();
		for(Iterable<String> i : a) {
			for(String s : i) {
				sb.append(s);
			}
		}
		return sb.toString();
	}


	public static interface TestRpc {
		public String m01();
		public String m02(String a1);
		public String m03(int a1);
		public String m04(List<String> a1);
		public String m05(List<List<String>> a1);
		public String m06(String a1, int a2);
	}

	public static class SyncHandler implements TestRpc {
		public String m01() {
			return "m01";
		}

		public String m02(String a1) {
			return "m02"+a1;
		}

		public String m03(int a1) {
			return "m03"+a1;
		}

		public String m04(List<String> a1) {
			return "m04"+stringify1(a1);
		}

		public String m05(List<List<String>> a1) {
			return "m05"+stringify2(a1);
		}

		public String m06(String a1, int a2) {
			return "m06"+a1+a2;
		}
	}

	public static class AsyncHandler {
        public void m01(Callback<String> callback) {
            callback.run("m01");
        }

        public void m02(Callback<String> callback, String a1) {
            callback.run("m02"+a1);
        }

        public void m03(Callback<String> callback, int a1) {
            callback.run("m03"+a1);
        }

        public void m04(Callback<String> callback, List<String> a1) {
            callback.run("m04"+stringify1(a1));
        }

        public void m05(Callback<String> callback, List<List<String>> a1) {
            callback.run("m05"+stringify2(a1));
        }

        public void m06(Callback<String> callback, String a1, int a2) {
            callback.run("m06"+a1+a2);
        }
    }

	static class Context {
		Server server;
		Client client;
		int port;
		Context(Server server, Client client, int port) {
			this.server = server;
			this.client = client;
			this.port = port;
		}
		Server getServer() { return server; }
		Client getClient() { return client; }
		int getPort() { return port; }
		void close() {
			server.close();
			client.close();
		}
	}

	int port = 19860;

	public synchronized Context startServer(Object handler) throws Exception {
		Server svr = new Server(EventLoop.defaultEventLoop());
		Client c = new Client("127.0.0.1", port);
		c.setRequestTimeout(10);
		try {
		    svr.serve(new ReflectionMethodDispatcher(handler,
		            MethodSelector.selectRpcServerMethod(handler.getClass())));
			svr.listen(port);
		} catch (Exception e) {
			svr.close();
			c.close();
			throw e;
		}
		return new Context(svr, c, port++);
	}

	public synchronized Context startServer2(Object handler) throws Exception {
	    Server svr = new Server(EventLoop.defaultEventLoop());
	    Client c = new Client("127.0.0.1", port);
	    c.setRequestTimeout(10);
	    try {
	        svr.serve(new JavassistMethodDispatcher(handler,
	                MethodSelector.selectRpcServerMethod(handler.getClass())));
	        svr.listen(port);
	    } catch (Exception e) {
	        svr.close();
	        c.close();
	        throw e;
	    }
	    return new Context(svr, c, port++);
	}

	static class ReflectionMethodDispatcher extends MethodDispatcher {
	    public ReflectionMethodDispatcher(Object target, Method[] methods) {
	        super(target, methods);
	        InvokerBuilder builder = ReflectionInvokerBuilder.getInstance();
            for(Method method : methods) {
                methodMap.put(method.getName(), builder.buildInvoker(method));
            }
	    }
	}

	static class JavassistMethodDispatcher extends MethodDispatcher {
        public JavassistMethodDispatcher(Object target, Method[] methods) {
            super(target, methods);
            InvokerBuilder builder = JavassistInvokerBuilder.getInstance();
            for(Method method : methods) {
                methodMap.put(method.getName(), builder.buildInvoker(method));
            }
        }
	}
}

