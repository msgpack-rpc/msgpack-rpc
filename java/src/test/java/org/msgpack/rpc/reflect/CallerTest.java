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

public class CallerTest extends ReflectTest {
	/*
	@Test
	public void testSyncClient() throws Exception {
		Caller<TestRpc> caller = ReflectionCallerBuilder.build(TestRpc.class);

		Context context = startServer(new SyncHandler());
		TestRpc c = caller.newProxyInstance(context.getClient());
		try {
			String r01 = c.m01();
			assertEquals("m01", r01);

			String r02 = c.m02("furuhashi");
			assertEquals("m02"+"furuhashi", r02);

			String r03 = c.m03(1978);
			assertEquals("m03"+1978, r03);

			List<String> list = new ArrayList<String>();
			list.add("sadayuki");
			list.add("kumofs");
			String r04 = c.m04(list);
			assertEquals("m04"+stringify1(list), r04);

			List<List<String>> alist = new ArrayList<List<String>>();
			List<String> alist_n1 = new ArrayList<String>();
			alist_n1.add("1");
			alist_n1.add("2");
			alist_n1.add("3");
			alist.add(alist_n1);
			List<String> alist_n2 = new ArrayList<String>();
			alist_n2.add("a");
			alist_n2.add("b");
			alist_n2.add("c");
			alist.add(alist_n2);
			String r05 = c.m05(alist);
			assertEquals("m05"+stringify2(alist), r05);

			String r06 = c.m06("viver", 2006);
			assertEquals("m06"+"viver"+2006, r06);

		} finally {
			context.close();
		}
	}
	*/
}

