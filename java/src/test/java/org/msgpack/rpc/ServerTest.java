package org.msgpack.rpc;

import junit.framework.*;
import org.junit.Test;
import org.msgpack.rpc.client.Client;
import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.server.Server;
import org.msgpack.rpc.server.TCPServer;

import static org.junit.Assert.*;

public class ServerTest extends TestCase {
    private Thread serverThread;
    private Server server;

    private void startServer(final Object handler) throws Exception {
        serverThread = new Thread() {
            public void run() {
                try {
                    server = new TCPServer("0.0.0.0", 19850, handler);
                    server.serv();
                } catch (Exception e) {
                    fail();
                }
            }
        };
        serverThread.start();
        Thread.sleep(1500);
    }
    
    private void stopServer() throws Exception {
        server.stop();
        try {
            serverThread.join();
        } catch (InterruptedException e) {}
    }
    
    @Test
    public void testIt() throws Exception {
        startServer(this);
        
        EventLoop loop = new EventLoop();
        try {
            Client c = new Client("localhost", 19850, loop);
            testInt(c);
            testFloat(c);
            testDouble(c);
            testNil(c);
            testBool(c);
            testString(c);
            c.close();
        } finally {
            loop.shutdown();
        }
        
        stopServer();
    }
    
    public int intFunc0() { return 0; }
    public int intFunc1(int a) { return a; }
    public int intFunc2(int a, int b) { return b; }
    protected void testInt(Client c) throws Exception {
        Object o;
        o = c.call("intFunc0");
        assertEquals(0, ((Number)o).intValue());
        o = c.call("intFunc1", 1);
        assertEquals(1, ((Number)o).intValue());
        o = c.call("intFunc2", 1, 2);
        assertEquals(2, ((Number)o).intValue());
    }
    
    public float floatFunc0() { return (float)0.0; }
    public float floatFunc1(Float a) { return a; }
    public float floatFunc2(Float a, Float b) { return b; }
    protected void testFloat(Client c) throws Exception {
        Object o;
        o = c.call("floatFunc0");
        assertEquals(0.0, ((Float)o).floatValue(), 10e-10);
        o = c.call("floatFunc1", (float)1.0);
        assertEquals(1.0, ((Float)o).floatValue(), 10e-10);
        o = c.call("floatFunc2", (float)1.0, (float)2.0);
        assertEquals(2.0, ((Float)o).floatValue(), 10e-10);
    }
    
    public double doubleFunc0() { return 0.0; }
    public double doubleFunc1(Double a) { return a; }
    public double doubleFunc2(Double a, Double b) { return b; }
    protected void testDouble(Client c) throws Exception {
        Object o;
        o = c.call("doubleFunc0");
        assertEquals(0.0, ((Double)o).doubleValue(), 10e-10);
        o = c.call("doubleFunc1", 1.0);
        assertEquals(1.0, ((Double)o).doubleValue(), 10e-10);
        o = c.call("doubleFunc2", 1.0, 2.0);
        assertEquals(2.0, ((Double)o).doubleValue(), 10e-10);
    }
    
    public Object nilFunc0() { return null; }
    public Object nilFunc1(Object a) { return a; }
    public Object nilFunc2(Object a, Object b) { return b; }
    protected void testNil(Client c) throws Exception {
        Object o;
        o = c.call("nilFunc0");
        assertEquals(null, o);
        o = c.call("nilFunc1", null);
        assertEquals(null, o);
        o = c.call("nilFunc2", null, null);
        assertEquals(null, o);
    }
    
    public Boolean boolFunc0() { return false; }
    public Boolean boolFunc1(Boolean a) { return a; }
    public Boolean boolFunc2(Boolean a, Boolean b) { return b; }
    protected void testBool(Client c) throws Exception {
        Object o;
        o = c.call("boolFunc0");
        assertEquals(false, ((Boolean)o).booleanValue());
        o = c.call("boolFunc1", false);
        assertEquals(false, ((Boolean)o).booleanValue());
        o = c.call("boolFunc2", false, true);
        assertEquals(true,  ((Boolean)o).booleanValue());
    }
    
    public String strFunc0() { return "0"; }
    public String strFunc1(byte[] a) { return new String(a); }
    public String strFunc2(byte[] a, byte[] b) { return new String(b); }
    protected void testString(Client c) throws Exception {
        Object o;
        o = c.call("strFunc0");
        assertEquals("0", new String((byte[])o));
        o = c.call("strFunc1", "1");
        assertEquals("1", new String((byte[])o));
        o = c.call("strFunc2", "1", "2");
        assertEquals("2", new String((byte[])o));
    }
}
