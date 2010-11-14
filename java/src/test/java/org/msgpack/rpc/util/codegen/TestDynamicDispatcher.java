package org.msgpack.rpc.util.codegen;

import java.io.IOException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.msgpack.CustomMessage;
import org.msgpack.MessageConvertable;
import org.msgpack.MessagePackObject;
import org.msgpack.MessagePackable;
import org.msgpack.MessageTypeException;
import org.msgpack.Packer;
import org.msgpack.Template;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Future;
import org.msgpack.rpc.Server;
import org.msgpack.util.codegen.DynamicTemplate;

public class TestDynamicDispatcher extends TestCase {

    private static EventLoop LOOP;

    private static Server SERVER;

    private static Client CLIENT;

    private static final String HOST = "localhost";

    private static final int PORT = 11311;
    
    private static final int LOOP_COUNT = 10;

    public TestDynamicDispatcher() {
        super();
    }

    @Before
    public void setUp() throws Exception {
        LOOP = new EventLoop();
        SERVER = new Server(LOOP);
        CLIENT = new Client(HOST, PORT);
    }

    @After
    public void tearDown() throws Exception {
        if (SERVER != null) {
            SERVER.close();
        }
        if (CLIENT != null) {
            CLIENT.close();
        }
        if (LOOP != null) {
            LOOP.shutdown();
        }
    }

    @Test
    public void testPrimitiveTypeHandler00() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            CLIENT.callApply("m0", new Object[0]);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m1", new Object[] { (byte) i, (byte) i + 1 });
            assertEquals((byte) i, ret.asByte());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m2", new Object[] { (short) i, (short) i + 1 });
            assertEquals((short) i, ret.asShort());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m3", new Object[] { i, i + 1 });
            assertEquals(i, ret.asInt());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m4", new Object[] { i, i + 1 });
            assertEquals(i, ret.asLong());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m5", new Object[] { (float) i, (float) i + 1 });
            assertEquals((float) i, ret.asFloat());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m6", new Object[] { (double) i, (double) i + 1 });
            assertEquals((double) i, ret.asDouble());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m7", new Object[] { i % 2 == 0, i % 2 != 0 });
            assertEquals(i % 2 == 0, ret.asBoolean());
        }
    }

    @Test
    public void testPrimitiveTypeHandler01() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            CLIENT.callAsyncApply("m0", new Object[0]);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m1", new Object[] { (byte) i, (byte) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((byte) i, ret.asByte());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m2", new Object[] { (short) i, (short) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((short) i, ret.asShort());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m3", new Object[] { i, i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i, ret.asInt());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m4", new Object[] { i, i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i, ret.asLong());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m5", new Object[] { (float) i, (float) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((float) i, ret.asFloat());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m6", new Object[] { (double) i, (double) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((double) i, ret.asDouble());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m7", new Object[] { i % 2 == 0, i % 2 != 0 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i % 2 == 0, ret.asBoolean());
        }
    }

    public static class PrimitiveTypeHandler {
        public void m0() {
        }

        public byte m1(byte p0, byte p1) {
            return p0;
        }

        public short m2(short p0, short p1) {
            return p0;
        }

        public int m3(int p0, int p1) {
            return p0;
        }

        public long m4(long p0, long p1) {
            return p0;
        }

        public float m5(float p0, float p1) {
            return p0;
        }

        public double m6(double p0, double p1) {
            return p0;
        }

        public boolean m7(boolean p0, boolean p1) {
            return p0;
        }
    }

    @Test
    public void testWrapperTypeHandler00() throws Exception {
        SERVER.serve(new DynamicDispatcher(new WrapperTypeHandler()));
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m0", new Object[] { (byte) i, (byte) i + 1 });
            assertEquals((byte) i, ret.asByte());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m1", new Object[] { (short) i, (short) i + 1 });
            assertEquals((short) i, ret.asShort());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m2", new Object[] { i, i + 1 });
            assertEquals(i, ret.asInt());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m3", new Object[] { i, i + 1 });
            assertEquals(i, ret.asLong());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m4", new Object[] { (float) i, (float) i + 1 });
            assertEquals((float) i, ret.asFloat());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m5", new Object[] { (double) i, (double) i + 1 });
            assertEquals((double) i, ret.asDouble());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m6", new Object[] { i % 2 == 0, i % 2 != 0 });
            assertEquals(i % 2 == 0, ret.asBoolean());
        }
    }

    @Test
    public void testWrapperTypeHandler01() throws Exception {
        SERVER.serve(new DynamicDispatcher(new WrapperTypeHandler()));
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m0", new Object[] { (byte) i, (byte) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((byte) i, ret.asByte());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m1", new Object[] { (short) i, (short) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((short) i, ret.asShort());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m2", new Object[] { i, i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i, ret.asInt());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m3", new Object[] { i, i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i, ret.asLong());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m4", new Object[] { (float) i, (float) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((float) i, ret.asFloat());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m5", new Object[] { (double) i, (double) i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals((double) i, ret.asDouble());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m6", new Object[] { i % 2 == 0, i % 2 != 0 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(i % 2 == 0, ret.asBoolean());
        }
    }

    public static class WrapperTypeHandler {
        public Byte m0(Byte p0, Byte p1) {
            return p0;
        }

        public Short m1(Short p0, Short p1) {
            return p0;
        }

        public Integer m2(Integer p0, Integer p1) {
            return p0;
        }

        public Long m3(Long p0, Long p1) {
            return p0;
        }

        public Float m4(Float p0, Float p1) {
            return p0;
        }

        public Double m5(Double p0, Double p1) {
            return p0;
        }

        public Boolean m6(Boolean p0, Boolean p1) {
            return p0;
        }
    }

    @Test
    public void testReferenceTypeHandler00() throws Exception {
        SERVER.serve(new DynamicDispatcher(new ReferenceTypeHandler()));
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m0", new Object[] { "muga" + i, "muga" + i + 1 });
            assertEquals("muga" + i, ret.asString());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = CLIENT.callApply("m1", new Object[] { BigInteger.valueOf(i), BigInteger.valueOf(i + 1) });
            assertEquals(BigInteger.valueOf(i), ret.asBigInteger());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            byte[] p0 = new byte[] { (byte) i };
            byte[] p1 = new byte[] { (byte) (i + 1) };
            MessagePackObject ret = CLIENT.callApply("m2", new Object[] { p0, p1 });
            assertEquals(p0[0], ret.asByteArray()[0]);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            List<String> p0 = new ArrayList<String>();
            p0.add("muga" + i);
            List<Integer> p1 = new ArrayList<Integer>();
            p1.add(i);
            MessagePackObject ret = CLIENT.callApply("m3", new Object[] { p0, p1 });
            List<MessagePackObject> list = ret.asList();
            assertEquals(p0.size(), list.size());
            assertEquals(p0.get(0), list.get(0).asString());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Map<String, Integer> p0 = new HashMap<String, Integer>();
            p0.put("muga" + i, i);
            Map<String, String> p1 = new HashMap<String, String>();
            p1.put("muga" + i, "muga" + i);
            MessagePackObject ret = CLIENT.callApply("m4", new Object[] { p0, p1 });
            Map<MessagePackObject, MessagePackObject> map = ret.asMap();
            assertEquals(p0.size(), map.size());
            Iterator<Entry<MessagePackObject, MessagePackObject>> iter = map.entrySet().iterator();
            assertTrue(iter.hasNext());
            Entry<MessagePackObject, MessagePackObject> e = iter.next();
            String k = e.getKey().asString();
            int v = e.getValue().asInt();
            assertEquals(p0.get(k).intValue(), v);
            assertFalse(iter.hasNext());
        }
    }

    @Test
    public void testReferenceTypeHandler01() throws Exception {
        SERVER.serve(new DynamicDispatcher(new ReferenceTypeHandler()));
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m0", new Object[] { "muga" + i, "muga" + i + 1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals("muga" + i, ret.asString());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            fs[i] = CLIENT.callAsyncApply("m1", new Object[] { BigInteger.valueOf(i), BigInteger.valueOf(i + 1) });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            assertEquals(BigInteger.valueOf(i), ret.asBigInteger());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            byte[] p0 = new byte[] { (byte) i };
            byte[] p1 = new byte[] { (byte) (i + 1) };
            fs[i] = CLIENT.callAsyncApply("m2", new Object[] { p0, p1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            byte[] p0 = new byte[] { (byte) i };
            MessagePackObject ret = fs[i].get();
            assertEquals(p0[0], ret.asByteArray()[0]);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            List<String> p0 = new ArrayList<String>();
            p0.add("muga" + i);
            List<Integer> p1 = new ArrayList<Integer>();
            p1.add(i);
            fs[i] = CLIENT.callAsyncApply("m3", new Object[] { p0, p1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            List<MessagePackObject> list = ret.asList();
            assertEquals(1, list.size());
            assertEquals("muga" + i, list.get(0).asString());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Map<String, Integer> p0 = new HashMap<String, Integer>();
            p0.put("muga" + i, i);
            Map<String, String> p1 = new HashMap<String, String>();
            p1.put("muga" + i, "muga" + i);
            fs[i] = CLIENT.callAsyncApply("m4", new Object[] { p0, p1 });
        }            
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject ret = fs[i].get();
            Map<MessagePackObject, MessagePackObject> map = ret.asMap();
            assertEquals(1, map.size());
            Iterator<Entry<MessagePackObject, MessagePackObject>> iter = map.entrySet().iterator();
            assertTrue(iter.hasNext());
            Entry<MessagePackObject, MessagePackObject> e = iter.next();
            @SuppressWarnings("unused")
            String k = e.getKey().asString();
            int v = e.getValue().asInt();
            assertEquals(i, v);
            assertFalse(iter.hasNext());
        }
    }

    public static class ReferenceTypeHandler {
        public String m0(String p0, String p1) {
            return p0;
        }

        public BigInteger m1(BigInteger p0, BigInteger p1) {
            return p0;
        }

        public byte[] m2(byte[] p0, byte[] p1) {
            return p0;
        }

        public List<String> m3(List<String> p0, List<Integer> p1) {
            return p0;
        }

        public Map<String, Integer> m4(Map<String, Integer> p0, Map<String, String> p1) {
            return p0;
        }
    }

    @Test
    public void testMesagePackableConvertableTypeHandler00() throws Exception {
        SERVER.serve(new DynamicDispatcher(new MessagePackableConvertableTypeHandler()));
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackableConvertable p0 = new MessagePackableConvertable();
            p0.f0 = i;
            MessagePackableConvertable p1 = new MessagePackableConvertable();
            p1.f0 = i + 1;
            MessagePackObject ret = CLIENT.callApply("m0", new Object[] { p0, p1 });
            MessagePackableConvertable r = new MessagePackableConvertable();
            r.messageConvert(ret);
            assertEquals(p0.f0, r.f0);
        }
    }

    @Test
    public void testMesagePackableConvertableTypeHandler01() throws Exception {
        SERVER.serve(new DynamicDispatcher(new MessagePackableConvertableTypeHandler()));
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackableConvertable p0 = new MessagePackableConvertable();
            p0.f0 = i;
            MessagePackableConvertable p1 = new MessagePackableConvertable();
            p1.f0 = i + 1;
            fs[i] = CLIENT.callAsyncApply("m0", new Object[] { p0, p1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackableConvertable p0 = new MessagePackableConvertable();
            p0.f0 = i;
            MessagePackObject ret = fs[i].get();
            MessagePackableConvertable r = new MessagePackableConvertable();
            r.messageConvert(ret);
            assertEquals(p0.f0, r.f0);
        }
    }

    public static class MessagePackableConvertableTypeHandler {
        public MessagePackableConvertable m0(MessagePackableConvertable p0,
                MessagePackableConvertable p1) {
            return p0;
        }
    }

    public static class MessagePackableConvertable implements MessagePackable,
            MessageConvertable {

        public int f0;

        public MessagePackableConvertable() {
        }

        @Override
        public void messagePack(Packer packer) throws IOException {
            packer.packArray(1);
            packer.packInt(f0);
        }

        @Override
        public void messageConvert(MessagePackObject obj)
                throws MessageTypeException {
            MessagePackObject[] objs = obj.asArray();
            f0 = objs[0].asInt();
        }
    }

    public void testUserDefinedTypeHandler00() throws Exception {
        Template tmpl = DynamicTemplate.create(UserDefinedType.class);
        CustomMessage.register(UserDefinedType.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new UserDefinedTypeHandler()));
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            UserDefinedType p0 = new UserDefinedType();
            p0.f0 = i;
            p0.f1 = i + 1;
            UserDefinedType p1 = new UserDefinedType();
            p1.f0 = i;
            p1.f1 = i + 1;
            MessagePackObject ret = CLIENT.callApply("m0", new Object[] { p0, p1 });
            UserDefinedType r = (UserDefinedType) tmpl.convert(ret, null);
            assertEquals(p0.f0, r.f0);
        }
    }

    public void testUserDefinedTypeHandler01() throws Exception {
        Template tmpl = DynamicTemplate.create(UserDefinedType.class);
        CustomMessage.register(UserDefinedType.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new UserDefinedTypeHandler()));
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            UserDefinedType p0 = new UserDefinedType();
            p0.f0 = i;
            p0.f1 = i + 1;
            UserDefinedType p1 = new UserDefinedType();
            p1.f0 = i;
            p1.f1 = i + 1;
            fs[i] = CLIENT.callAsyncApply("m0", new Object[] { p0, p1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackObject mpo = fs[i].get();
            UserDefinedType r = (UserDefinedType) tmpl.convert(mpo, null);
            assertEquals(i, r.f0);
        }
    }

    public static class UserDefinedTypeHandler {
        public UserDefinedType m0(UserDefinedType p0, UserDefinedType p1) {
            return p0;
        }
    }

    public static class UserDefinedType {
        public int f0;

        public int f1; 

        public UserDefinedType() {
        }
    }

    //@Test
    public void XtestApplicationExceptionHandler00() throws Exception {
        Template tmpl = DynamicTemplate.create(ApplicationException.class);
        CustomMessage.register(ApplicationException.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new ApplicationExceptionHandler()));  
        SERVER.listen(PORT);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            try {
                int p0 = i;
                int p1 = i + 1;
                @SuppressWarnings("unused")
                MessagePackObject ret = CLIENT.callApply("m0", new Object[] { p0, p1 });
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    //@Test
    public void XtestApplicationExceptionHandler01() throws Exception {
        Template tmpl = DynamicTemplate.create(ApplicationException.class);
        CustomMessage.register(ApplicationException.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new ApplicationExceptionHandler()));  
        SERVER.listen(PORT);
        Future[] fs = new Future[LOOP_COUNT];
        for (int i = 0; i < LOOP_COUNT; ++i) {
            int p0 = i;
            int p1 = i + 1;
            fs[i] = CLIENT.callAsyncApply("m0", new Object[] { p0, p1 });
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            try {
                @SuppressWarnings("unused")
                MessagePackObject ret = fs[i].get();
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    public static class ApplicationExceptionHandler {
        public int m0(int p0, int p1) throws ApplicationException {
            throw new ApplicationException();
        }
    }

    @SuppressWarnings("serial")
    public static class ApplicationException extends Exception {
    }
}
