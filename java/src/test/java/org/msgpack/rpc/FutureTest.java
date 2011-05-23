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

public class FutureTest extends TestCase {
	public static class TestHandler {
		public TestHandler() { }
		public String m1(String a1) {
			return "ok"+a1;
		}
	}

	public interface TestInterface {
		public Future<String> m1(String a1);
		public Future<String> m1Async(String a1);  // /Async$/ will be removed
	}

	@Test
	public void testFuture() throws Exception {
		EventLoop loop = EventLoop.start();

		Server svr = new Server(loop);
		svr.serve(new TestHandler());
		svr.listen(19860);

		Client cli = new Client("127.0.0.1", 19860, loop);
		TestInterface c = cli.proxy(TestInterface.class);

		try {
			Future<String> f1 = c.m1("a1");
			Future<String> f2 = c.m1("a2");
			Future<String> f3 = c.m1Async("a3");

			f3.join();
			f1.join();
			f2.join();

			assertEquals(f1.get(), "ok"+"a1");
			assertEquals(f2.get(), "ok"+"a2");
			assertEquals(f3.get(), "ok"+"a3");

		} finally {
			svr.close();
			cli.close();
			loop.shutdown();
		}
	}
}

