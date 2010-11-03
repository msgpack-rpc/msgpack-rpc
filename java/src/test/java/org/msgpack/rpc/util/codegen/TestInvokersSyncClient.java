package org.msgpack.rpc.util.codegen;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.msgpack.CustomMessage;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.Server;
import org.msgpack.util.codegen.DynamicTemplate;

public class TestInvokersSyncClient extends TestCase {

    private static EventLoop LOOP;

    private static Server SERVER;

    private static Client CLIENT;

    private static final String HOST = "localhost";

    private static final int PORT = 11311;

    private static final int LOOP_COUNT = 10;

    public TestInvokersSyncClient() {
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
    public void testFoo01() throws Exception {
        CustomMessage.register(Foo.class, DynamicTemplate.create(Foo.class));
        SERVER.serve(new DynamicDispatcher(ITestHandler.class,
                new TestHandler()));
        SERVER.listen(PORT);
    }

    @Test
    public void testFoo02() throws Exception {
        CustomMessage.register(Foo.class, DynamicTemplate.create(Foo.class));
        SERVER.serve(new DynamicDispatcher(ITestHandler.class,
                new TestHandler()));
        SERVER.listen(PORT);
    }

    public static class Foo {
        public int i;

        public int j;

        public Foo() {
        }
    }

    public static interface ITestHandler {
        int m0(int i, int j);

        void m1(Request req);

        void m2(Request req, int i, int j);

        int m3(Foo foo);

        List<Integer> m4(List<Integer> list);
    }

    public static interface ITestHandler2 {
        int m0(int i, int j);

        int m3(Foo foo);

        List<Integer> m4(List<Integer> list);

        void m5(int i, int j);
    }

    public static class TestHandler implements ITestHandler {
        public int m0(int i, int j) {
            return i + j;
        }

        public void m1(Request req) {
            MessagePackObject packObj = req.getArguments();
            MessagePackObject[] packObjs = packObj.asArray();
            int ret = packObjs[0].intValue() + packObjs[1].intValue();
            req.sendResponse(ret, null);
        }

        public void m2(Request req, int i, int j) {
            req.sendResponse(i + j, null);
        }

        public int m3(Foo foo) {
            return foo.i + foo.j;
        }

        public List<Integer> m4(List<Integer> list) {
            List<Integer> ret = new ArrayList<Integer>();
            int i = list.get(0);
            int j = list.get(1);
            ret.add(i + j);
            return ret;
        }

        public void m5(int i, int j) {
            @SuppressWarnings("unused")
            int ret = i + j;
        }
    }

    @Test
    public void testPrimitiveTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
        IPrimitiveTypeHandler c = DynamicSyncClient.create(CLIENT, IPrimitiveTypeHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            c.m0();
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            byte ret = c.m1((byte) i, (byte) (i + 1));
            assertEquals((byte) i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            short ret = c.m2((short) i, (short) (i + 1));
            assertEquals((short) i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            int ret = c.m3(i, i + 1);
            assertEquals(i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            long ret = c.m4(i, i + 1);
            assertEquals(i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            float ret = c.m5((float) i, (float) (i + 1));
            assertEquals((float) i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            double ret = c.m6((double) i, (double) (i + 1));
            assertEquals((double) i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            boolean ret = c.m7(i % 2 == 0, i % 2 != 0);
            assertEquals(i % 2 == 0, ret);
        }
    }

    public static interface IPrimitiveTypeHandler {
        void m0();
        byte m1(byte p0, byte p1);
        short m2(short p0, short p1);
        int m3(int p0, int p1);
        long m4(long p0, long p1);
        float m5(float p0, float p1);
        double m6(double p0, double p1);
        boolean m7(boolean p0, boolean p1);
    }

    public static class PrimitiveTypeHandler implements IPrimitiveTypeHandler {
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
    public void testWrapperTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new WrapperTypeHandler()));
        SERVER.listen(PORT);
        IWrapperTypeHandler c = DynamicSyncClient.create(CLIENT, IWrapperTypeHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Byte ret = c.m0((byte) i, (byte) (i + 1));
            assertEquals((byte) i, ret.byteValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Short ret = c.m1((short) i, (short) (i + 1));
            assertEquals((short) i, ret.shortValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Integer ret = c.m2(i, i + 1);
            assertEquals(i, ret.intValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Long ret = c.m3((long) i, (long) (i + 1));
            assertEquals((long) i, ret.longValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Float ret = c.m4((float) i, (float) (i + 1));
            assertEquals((float) i, ret.floatValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Double ret = c.m5((double) i, (double) (i + 1));
            assertEquals((double) i, ret.doubleValue());
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            Boolean ret = c.m6(i % 2 == 0, i % 2 != 0);
            assertEquals(i % 2 == 0, ret.booleanValue());
        }
    }

    public static interface IWrapperTypeHandler {
        Byte m0(Byte p0, Byte p1);
        Short m1(Short p0, Short p1);
        Integer m2(Integer p0, Integer p1);
        Long m3(Long p0, Long p1);
        Float m4(Float p0, Float p1);
        Double m5(Double p0, Double p1);
        Boolean m6(Boolean p0, Boolean p1);
    }

    public static class WrapperTypeHandler implements IWrapperTypeHandler {
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
    public void testReferenceTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new ReferenceTypeHandler()));
        SERVER.listen(PORT);
        IReferenceTypeHandler c = DynamicSyncClient.create(CLIENT, IReferenceTypeHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            String ret = c.m0("muga" + i, "muga" + i + 1);
            assertEquals("muga" + i, ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            BigInteger ret = c.m1(BigInteger.valueOf(i), BigInteger.valueOf(i + 1));
            assertEquals(BigInteger.valueOf(i), ret);
        }
        for (int i = 0; i < LOOP_COUNT; ++i) {
            byte[] p0 = new byte[] { (byte) i };
            byte[] p1 = new byte[] { (byte) (i + 1) };
            byte[] ret = c.m2(p0, p1);
            assertEquals(p0[0], ret[0]);
        }
    }

    public static interface IReferenceTypeHandler {
        String m0(String p0, String p1);
        BigInteger m1(BigInteger p0, BigInteger p1);
        byte[] m2(byte[] p0, byte[] p1);
    }

    public static class ReferenceTypeHandler implements IReferenceTypeHandler {
        public String m0(String p0, String p1) {
            return p0;
        }

        public BigInteger m1(BigInteger p0, BigInteger p1) {
            return p0;
        }

        public byte[] m2(byte[] p0, byte[] p1) {
            return p0;
        }
    }
}
