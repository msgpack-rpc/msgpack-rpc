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
import org.msgpack.annotation.*;
import org.msgpack.rpc.*;
import org.msgpack.rpc.dispatcher.*;
import org.msgpack.rpc.config.*;
import org.msgpack.rpc.loop.*;
import org.msgpack.rpc.loop.netty.*;
import java.util.*;
import junit.framework.*;
import org.junit.Test;

public class AnnotationTest extends ReflectTest {
	public static interface IgnoreTest {
		public String m01(String a0, @Ignore String a1);
		public String m02(@Ignore String a0, String a1);
	}

	public static interface OmitTest {
		public String m01(String a0);
		public String m02(String a0);
	}

	public static class IgnoreTestHandler implements IgnoreTest {
		public IgnoreTestHandler() { }
		public String m01(String a0, @Ignore String a1) {
			return ""+a0+a1;
		}
		public String m02(@Ignore String a0, String a1) {
			return ""+a0+a1;
		}
	}

	public static class OmitTestHandler implements OmitTest {
		public OmitTestHandler() { }
		public String m01(String a0) {
			return a0;
		}
		public String m02(String a0) {
			return a0;
		}
	}

	@Test
	public void testIgnoreClientOmitServer() throws Exception {
		Context context = startServer(new OmitTestHandler());
		IgnoreTest c = context.getClient().proxy(IgnoreTest.class);
		try {
			String result;

			result = c.m01("a0", "a1");
			assertEquals("a0", result);

			result = c.m02("a0", "a1");
			assertEquals("a1", result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testIgnoreClientOmitServer2() throws Exception {
        Context context = startServer2(new OmitTestHandler());
        IgnoreTest c = context.getClient().proxy(IgnoreTest.class);
        try {
            String result;

            result = c.m01("a0", "a1");
            assertEquals("a0", result);

            result = c.m02("a0", "a1");
            assertEquals("a1", result);

        } finally {
            context.close();
        }
    }

    @Test
	public void testOmitClientIgnoreServer() throws Exception {
		Context context = startServer(new IgnoreTestHandler());
		OmitTest c = context.getClient().proxy(OmitTest.class);
		try {
			String result;
			
			result = c.m01("a0");
			assertEquals(""+"a0"+null, result);

			result = c.m02("a0");
			assertEquals(""+null+"a0", result);

		} finally {
			context.close();
		}
	}

    @Test
    public void testOmitClientIgnoreServer2() throws Exception {
        Context context = startServer2(new IgnoreTestHandler());
        OmitTest c = context.getClient().proxy(OmitTest.class);
        try {
            String result;
            
            result = c.m01("a0");
            assertEquals(""+"a0"+null, result);

            result = c.m02("a0");
            assertEquals(""+null+"a0", result);

        } finally {
            context.close();
        }
    }

	public static interface OptionalTestV1 {
		public String m01(String a0);
	}

	public static interface OptionalTestV2 {
		public String m01(String a0, @Optional String a1);
	}

	public static class OptionalTestHandler implements OptionalTestV2 {
		public OptionalTestHandler() { }
		public String m01(String a0, @Optional String a1) {
			return ""+a0+a1;
		}
	}

	@Test
	public void testOptionalV1() throws Exception {
		Context context = startServer(new OptionalTestHandler());
		OptionalTestV1 c = context.getClient().proxy(OptionalTestV1.class);
		try {
			String result;

			result = c.m01("a0");
			assertEquals(""+"a0"+null, result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testOptionalV12() throws Exception {
        Context context = startServer2(new OptionalTestHandler());
        OptionalTestV1 c = context.getClient().proxy(OptionalTestV1.class);
        try {
            String result;

            result = c.m01("a0");
            assertEquals(""+"a0"+null, result);

        } finally {
            context.close();
        }
    }

	@Test
	public void testOptionalV2() throws Exception {
		Context context = startServer(new OptionalTestHandler());
		OptionalTestV2 c = context.getClient().proxy(OptionalTestV2.class);
		try {
			String result;

			result = c.m01("a0", "a1");
			assertEquals(""+"a0"+"a1", result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testOptionalV22() throws Exception {
        Context context = startServer2(new OptionalTestHandler());
        OptionalTestV2 c = context.getClient().proxy(OptionalTestV2.class);
        try {
            String result;

            result = c.m01("a0", "a1");
            assertEquals(""+"a0"+"a1", result);

        } finally {
            context.close();
        }
    }

	public static interface IndexTestV1 {
		public String m01(String a0);
		public String m02(@Optional String a0);
		public String m03(@Optional String a0);
	}

	public static interface IndexTestV2 {
		public String m01(@Optional @Index(1) String a1, @Index(0) String a0);
		public String m02(@Index(1) @Optional String a1, @Optional String a2);
		public String m03(@Index(1) @Optional String a1, @Index(0) @Optional String a0);
	}

	public static interface IndexTestV2Full {
		public String m01(String a0, @Optional String a1);
		public String m02(@Optional String a0, @Optional String a1, @Optional String a2);
		public String m03(@Optional String a0, @Optional String a1);
	}

	public static class IndexTestHandlerV1 implements IndexTestV1 {
		public IndexTestHandlerV1() { }
		public String m01(String a0) {
			return ""+a0;
		}
		public String m02(@Optional String a0) {
			return ""+a0;
		}
		public String m03(@Optional String a0) {
			return ""+a0;
		}
	}

	public static class IndexTestHandlerV2 {
		public IndexTestHandlerV2() { }
		public String m01(@Optional @Index(1) String a1, @Index(0) String a0) {
			return ""+a0+a1;
		}
		public String m02(@Index(1) @Optional String a1, @Optional String a2) {
			return ""+a1+a2;
		}
		public String m03(@Index(1) @Optional String a1, @Index(0) @Optional String a0) {
			return ""+a0+a1;
		}
	}

	@Test
	public void testIndexV1ClientV2Server() throws Exception {
		Context context = startServer(new IndexTestHandlerV2());
		IndexTestV1 c = context.getClient().proxy(IndexTestV1.class);
		try {
			String result;

			result = c.m01("a0");
			assertEquals(""+"a0"+null, result);

			result = c.m02("a0");
			assertEquals(""+null+null, result);

			result = c.m03("a0");
			assertEquals(""+"a0"+null, result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testIndexV1ClientV2Server2() throws Exception {
        Context context = startServer2(new IndexTestHandlerV2());
        IndexTestV1 c = context.getClient().proxy(IndexTestV1.class);
        try {
            String result;

            result = c.m01("a0");
            assertEquals(""+"a0"+null, result);

            result = c.m02("a0");
            assertEquals(""+null+null, result);

            result = c.m03("a0");
            assertEquals(""+"a0"+null, result);

        } finally {
            context.close();
        }
    }

	@Test
	public void testIndexV2ClientV1Server() throws Exception {
		Context context = startServer(new IndexTestHandlerV1());
		IndexTestV2 c = context.getClient().proxy(IndexTestV2.class);
		try {
			String result;

			result = c.m01("a1", "a0");
			assertEquals(""+"a0", result);

			result = c.m02("a1", "a2");
			assertEquals(""+null, result);

			result = c.m03("a1", "a0");
			assertEquals(""+"a0", result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testIndexV2ClientV1Server2() throws Exception {
        Context context = startServer2(new IndexTestHandlerV1());
        IndexTestV2 c = context.getClient().proxy(IndexTestV2.class);
        try {
            String result;

            result = c.m01("a1", "a0");
            assertEquals(""+"a0", result);

            result = c.m02("a1", "a2");
            assertEquals(""+null, result);

            result = c.m03("a1", "a0");
            assertEquals(""+"a0", result);

        } finally {
            context.close();
        }
    }

	@Test
	public void testIndexV2FullClientV2Server() throws Exception {
		Context context = startServer(new IndexTestHandlerV2());
		IndexTestV2Full c = context.getClient().proxy(IndexTestV2Full.class);
		try {
			String result;

			result = c.m01("a0", "a1");
			assertEquals(""+"a0"+"a1", result);

			result = c.m02("a0", "a1", "a2");
			assertEquals(""+"a1"+"a2", result);

			result = c.m03("a0", "a1");
			assertEquals(""+"a0"+"a1", result);

		} finally {
			context.close();
		}
	}

	@Test
    public void testIndexV2FullClientV2Server2() throws Exception {
        Context context = startServer2(new IndexTestHandlerV2());
        IndexTestV2Full c = context.getClient().proxy(IndexTestV2Full.class);
        try {
            String result;

            result = c.m01("a0", "a1");
            assertEquals(""+"a0"+"a1", result);

            result = c.m02("a0", "a1", "a2");
            assertEquals(""+"a1"+"a2", result);

            result = c.m03("a0", "a1");
            assertEquals(""+"a0"+"a1", result);

        } finally {
            context.close();
        }
    }
}

