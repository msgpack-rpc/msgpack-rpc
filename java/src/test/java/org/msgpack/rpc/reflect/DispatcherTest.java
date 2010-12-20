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

public class DispatcherTest extends ReflectTest {
	@Test
	public void testSyncHandler() throws Exception {
		Context context = startServer(new SyncHandler());
		Client c = context.getClient();
		try {
			MessagePackObject result;

			result = c.callApply("m01", new Object[]{});
			assertTrue(result.isRawType());
			assertEquals("m01", result.asString());

			result = c.callApply("m02", new Object[]{"furuhashi"});
			assertTrue(result.isRawType());
			assertEquals("m02"+"furuhashi", result.asString());

			result = c.callApply("m03", new Object[]{1978});
			assertTrue(result.isRawType());
			assertEquals("m03"+1978, result.asString());

			List<String> list = new ArrayList<String>();
			list.add("sadayuki");
			list.add("kumofs");
			result = c.callApply("m04", new Object[]{list});
			assertTrue(result.isRawType());
			assertEquals("m04"+stringify1(list), result.asString());

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
			result = c.callApply("m05", new Object[]{alist});
			assertTrue(result.isRawType());
			assertEquals("m05"+stringify2(alist), result.asString());

			result = c.callApply("m06", new Object[]{"viver", 2006});
			assertTrue(result.isRawType());
			assertEquals("m06"+"viver"+2006, result.asString());

		} finally {
			context.close();
		}
	}

	@Test
    public void testSyncHandler2() throws Exception {
        Context context = startServer2(new SyncHandler());
        Client c = context.getClient();
        try {
            MessagePackObject result;

            result = c.callApply("m01", new Object[]{});
            assertTrue(result.isRawType());
            assertEquals("m01", result.asString());

            result = c.callApply("m02", new Object[]{"furuhashi"});
            assertTrue(result.isRawType());
            assertEquals("m02"+"furuhashi", result.asString());

            result = c.callApply("m03", new Object[]{1978});
            assertTrue(result.isRawType());
            assertEquals("m03"+1978, result.asString());

            List<String> list = new ArrayList<String>();
            list.add("sadayuki");
            list.add("kumofs");
            result = c.callApply("m04", new Object[]{list});
            assertTrue(result.isRawType());
            assertEquals("m04"+stringify1(list), result.asString());

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
            result = c.callApply("m05", new Object[]{alist});
            assertTrue(result.isRawType());
            assertEquals("m05"+stringify2(alist), result.asString());

            result = c.callApply("m06", new Object[]{"viver", 2006});
            assertTrue(result.isRawType());
            assertEquals("m06"+"viver"+2006, result.asString());

        } finally {
            context.close();
        }
    }

    @Test
	public void testAsyncHandler() throws Exception {
		Context context = startServer(new AsyncHandler());
		Client c = context.getClient();
		try {
			MessagePackObject result;

			result = c.callApply("m01", new Object[]{});
			assertTrue(result.isRawType());
			assertEquals("m01", result.asString());

			result = c.callApply("m02", new Object[]{"furuhashi"});
			assertTrue(result.isRawType());
			assertEquals("m02"+"furuhashi", result.asString());

			result = c.callApply("m03", new Object[]{1978});
			assertTrue(result.isRawType());
			assertEquals("m03"+1978, result.asString());

			List<String> list = new ArrayList<String>();
			list.add("sadayuki");
			list.add("kumofs");
			result = c.callApply("m04", new Object[]{list});
			assertTrue(result.isRawType());
			assertEquals("m04"+stringify1(list), result.asString());

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
			result = c.callApply("m05", new Object[]{alist});
			assertTrue(result.isRawType());
			assertEquals("m05"+stringify2(alist), result.asString());

			result = c.callApply("m06", new Object[]{"viver", 2006});
			assertTrue(result.isRawType());
			assertEquals("m06"+"viver"+2006, result.asString());

		} finally {
			context.close();
		}
	}

    @Test
    public void testAsyncHandler2() throws Exception {
        Context context = startServer2(new AsyncHandler());
        Client c = context.getClient();
        try {
            MessagePackObject result;

            result = c.callApply("m01", new Object[]{});
            assertTrue(result.isRawType());
            assertEquals("m01", result.asString());

            result = c.callApply("m02", new Object[]{"furuhashi"});
            assertTrue(result.isRawType());
            assertEquals("m02"+"furuhashi", result.asString());

            result = c.callApply("m03", new Object[]{1978});
            assertTrue(result.isRawType());
            assertEquals("m03"+1978, result.asString());

            List<String> list = new ArrayList<String>();
            list.add("sadayuki");
            list.add("kumofs");
            result = c.callApply("m04", new Object[]{list});
            assertTrue(result.isRawType());
            assertEquals("m04"+stringify1(list), result.asString());

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
            result = c.callApply("m05", new Object[]{alist});
            assertTrue(result.isRawType());
            assertEquals("m05"+stringify2(alist), result.asString());

            result = c.callApply("m06", new Object[]{"viver", 2006});
            assertTrue(result.isRawType());
            assertEquals("m06"+"viver"+2006, result.asString());

        } finally {
            context.close();
        }
    }
}

