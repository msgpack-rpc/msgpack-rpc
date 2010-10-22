package org.msgpack.rpc.util.codegen;

import junit.framework.TestCase;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Server;
import org.msgpack.rpc.util.codegen.TestFoo.ITestHandler;
import org.msgpack.rpc.util.codegen.TestFoo.TestHandler;

public class TestInvokersSyncClient extends TestCase {

    private static EventLoop LOOP;
    
    private static Server SERVER;
    
    private static Client CLIENT;
    
    private static final String HOST = "localhost";
    
    private static final int PORT = 11311;
    
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
        SERVER.close();
        CLIENT.close();
        LOOP.shutdown();
    }
    
    @Test
    public void testFoo01() throws Exception {
        SERVER.serve(new DynamicDispatcher(ITestHandler.class, new TestHandler()));
        SERVER.listen(PORT);
    }
    
    @Test
    public void testFoo02() throws Exception {
        SERVER.serve(new DynamicDispatcher(ITestHandler.class, new TestHandler()));
        SERVER.listen(PORT);
    }
}
