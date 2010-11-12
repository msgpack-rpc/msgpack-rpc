package org.msgpack.rpc.util.codegen;

import java.io.IOException;
import java.math.BigInteger;

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
import org.msgpack.rpc.Server;
import org.msgpack.util.codegen.DynamicTemplate;

public class TestDynamicDispatcherAndSyncClient extends TestCase {

    private static EventLoop LOOP;

    private static Server SERVER;

    private static Client CLIENT;

    private static final String HOST = "localhost";

    private static final int PORT = 11311;

    private static final int LOOP_COUNT = 10;

    public TestDynamicDispatcherAndSyncClient() {
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

    @Test
    public void testMesagePackableConvertableTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new MessagePackableConvertableTypeHandler()));
        SERVER.listen(PORT);
        IMessagePackableConvertableTypeHandler cc =
            DynamicSyncClient.create(CLIENT, IMessagePackableConvertableTypeHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            MessagePackableConvertable p0 = new MessagePackableConvertable();
            p0.f0 = i;
            MessagePackableConvertable p1 = new MessagePackableConvertable();
            p1.f0 = i + 1;
            MessagePackableConvertable r = cc.m0(p0, p1);
            assertEquals(p0.f0, r.f0);
        }
    }

    public static interface IMessagePackableConvertableTypeHandler {
        public MessagePackableConvertable m0(MessagePackableConvertable p0,
                MessagePackableConvertable p1);
    }

    public static class MessagePackableConvertableTypeHandler
        implements IMessagePackableConvertableTypeHandler {
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

    public void testUserDefinedTypeHandler() throws Exception {
        Template tmpl = DynamicTemplate.create(UserDefinedType.class);
        CustomMessage.register(UserDefinedType.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new UserDefinedTypeHandler()));
        SERVER.listen(PORT);
        IUserDefinedTypeHandler cc =
            DynamicSyncClient.create(CLIENT, IUserDefinedTypeHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            UserDefinedType p0 = new UserDefinedType();
            p0.f0 = i;
            p0.f1 = i + 1;
            UserDefinedType p1 = new UserDefinedType();
            p1.f0 = i;
            p1.f1 = i + 1;
            UserDefinedType r = cc.m0(p0, p1);
            assertEquals(p0.f0, r.f0);
        }
    }

    public static interface IUserDefinedTypeHandler {
        public UserDefinedType m0(UserDefinedType p0, UserDefinedType p1);
    }

    public static class UserDefinedTypeHandler implements IUserDefinedTypeHandler {
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
    public void XtestApplicationExceptionHandler() throws Exception {
        Template tmpl = DynamicTemplate.create(ApplicationException.class);
        CustomMessage.register(ApplicationException.class, tmpl);
        SERVER.serve(new DynamicDispatcher(new ApplicationExceptionHandler()));  
        SERVER.listen(PORT);
        IApplicationExceptionHandler cc =
            DynamicSyncClient.create(CLIENT, IApplicationExceptionHandler.class);
        for (int i = 0; i < LOOP_COUNT; ++i) {
            try {
                int p0 = i;
                int p1 = i + 1;
                @SuppressWarnings("unused")
                int r = cc.m0(p0, p1);
            } catch (Throwable t) {
                t.printStackTrace();
            }
        }
    }

    public static interface IApplicationExceptionHandler {
        public int m0(int p0, int p1) throws ApplicationException;
    }

    public static class ApplicationExceptionHandler
        implements IApplicationExceptionHandler {
        public int m0(int p0, int p1) throws ApplicationException {
            throw new ApplicationException();
        }
    }

    @SuppressWarnings("serial")
    public static class ApplicationException extends Exception {
    }
}
