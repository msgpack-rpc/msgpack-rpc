package org.msgpack.rpc.util.codegen;

import junit.framework.TestCase;

import org.junit.Test;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.Server;

public class ServerTest extends TestCase {
    public static class TestHandler {
        public int m0(int i, int j) {
            return i + j;
        }

        public void m1(Request req) {
            MessagePackObject[] packObjs = req.getArguments();
            int ret = packObjs[0].intValue() + packObjs[1].intValue();
            req.sendResponse(ret, null);
        }
        
        public void m2(Request req, int i, int j) {
            req.sendResponse(i + j, null);
        }
    }

    @Test
    public void testSyncLoad() throws Exception {
        EventLoop loop = new EventLoop();
        Server svr = new Server(loop);
        Client c = new Client("127.0.0.1", 19850);

        try {
            svr.serve(new DynamicCodegenDispatcher(new TestHandler()));
            svr.listen(19850);

            int num = 100;

            long start = System.currentTimeMillis();
            for (int i = 0; i < num; i++) {
                MessagePackObject ret0 = c.callApply("m0", new Object[] { i, i + 1 });
                assertEquals(2 * i + 1, ret0.intValue());
                MessagePackObject ret1 = c.callApply("m1", new Object[] { i, i + 1 });
                assertEquals(2 * i + 1, ret1.intValue());
                MessagePackObject ret2 = c.callApply("m2", new Object[] { i, i + 1 });
                assertEquals(2 * i + 1, ret2.intValue());
            }
            long finish = System.currentTimeMillis();

            double result = num / ((double) (finish - start) / 1000);
            System.out.println("sync: " + result + " calls per sec");

        } finally {
            svr.close();
            c.close();
            loop.shutdown();
        }
    }
    
    @Test
    public void testAsyncLoad() throws Exception {
        EventLoop loop = new EventLoop();
        Server svr = new Server(loop);
        Client c = new Client("127.0.0.1", 19850);

        try {
            svr.serve(new DynamicCodegenDispatcher(new TestHandler()));
            svr.listen(19850);

            int num = 100;

            long start = System.currentTimeMillis();
            for(int i=0; i < num-1; i++) {
                c.notifyApply("m0", new Object[] { i, i + 1 });
                c.notifyApply("m1", new Object[] { i, i + 1 });
                c.notifyApply("m2", new Object[] { i, i + 1 });
            }
            MessagePackObject ret0 = c.callApply("m0", new Object[] { 0, 1 });
            long finish = System.currentTimeMillis();

            double result = num / ((double)(finish - start) / 1000);
            System.out.println("async: "+result+" calls per sec");

        } finally {
            svr.close();
            c.close();
            loop.shutdown();
        }
    }
}
