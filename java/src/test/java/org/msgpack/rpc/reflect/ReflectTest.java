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

import org.msgpack.*;
import org.msgpack.object.*;
import org.msgpack.rpc.*;
import org.msgpack.rpc.dispatcher.*;
import org.msgpack.rpc.config.*;
import org.msgpack.rpc.loop.*;
import org.msgpack.rpc.loop.netty.*;
import java.util.*;
import junit.framework.*;
import org.junit.Test;

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
		public void m01(Request request) {
			request.sendResult("m01");
		}

		public void m02(Request request, String a1) {
			request.sendResult("m02"+a1);
		}

		public void m03(Request request, int a1) {
			request.sendResult("m03"+a1);
		}

		public void m04(Request request, List<String> a1) {
			request.sendResult("m04"+stringify1(a1));
		}

		public void m05(Request request, List<List<String>> a1) {
			request.sendResult("m05"+stringify2(a1));
		}

		public void m06(Request request, String a1, int a2) {
			request.sendResult("m06"+a1+a2);
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

	int port = 19850;

	public synchronized Context startServer(Object handler) throws Exception {
		Server svr = new Server(EventLoop.defaultEventLoop());
		Client c = new Client("127.0.0.1", port);
		c.setRequestTimeout(10);
		try {
			svr.serve(new MethodDispatcher(handler));
			svr.listen(port);
		} catch (Exception e) {
			svr.close();
			c.close();
			throw e;
		}
		return new Context(svr, c, port++);
	}
}

