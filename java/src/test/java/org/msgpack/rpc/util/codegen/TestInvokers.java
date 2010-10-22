package org.msgpack.rpc.util.codegen;

import java.io.IOException;
import java.math.BigInteger;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.msgpack.MessageConvertable;
import org.msgpack.MessagePackObject;
import org.msgpack.MessagePackable;
import org.msgpack.MessageTypeException;
import org.msgpack.Packer;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Server;

public class TestInvokers extends TestCase {

    private static EventLoop LOOP;

    private static Server SERVER;

    private static Client CLIENT;

    private static final String HOST = "localhost";

    private static final int PORT = 11311;

    public TestInvokers() {
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
        SERVER.close();
        CLIENT.close();
        LOOP.shutdown();
    }

    @Test
    public void testPrimitiveTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
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
    public void testWrapperTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
    }

    public static class WrapperTypeHandler {
        public void m0() {
        }

        public Byte m1(Byte p0, Byte p1) {
            return p0;
        }

        public Short m2(Short p0, Short p1) {
            return p0;
        }

        public Integer m3(Integer p0, Integer p1) {
            return p0;
        }

        public Long m4(Long p0, Long p1) {
            return p0;
        }

        public Float m5(Float p0, Float p1) {
            return p0;
        }

        public Double m6(Double p0, Double p1) {
            return p0;
        }

        public Boolean m7(Boolean p0, Boolean p1) {
            return p0;
        }
    }

    @Test
    public void testReferenceTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
    }

    public static class ReferenceTypeHandler {
        public void m0() {
        }

        public String m1(String p0, String p1) {
            return p0;
        }

        public BigInteger m2(BigInteger p0, BigInteger p1) {
            return p0;
        }
    }

    @Test
    public void testMesagePackableConvertableTypeHandler() throws Exception {
        SERVER.serve(new DynamicDispatcher(new PrimitiveTypeHandler()));
        SERVER.listen(PORT);
    }

    public static class MessagePackableConvertableTypeHandler {
        public void m0() {
        }

        public MessagePackableConvertable m1(MessagePackableConvertable p0,
                MessagePackableConvertable p1) {
            return p0;
        }
    }

    public static class MessagePackableConvertable implements MessagePackable,
            MessageConvertable {

        @Override
        public void messagePack(Packer packer) throws IOException {
        }

        @Override
        public void messageConvert(MessagePackObject obj)
                throws MessageTypeException {
        }



    }
}
