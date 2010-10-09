package org.msgpack.rpc.util.codegen;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.junit.Test;
import org.msgpack.CustomMessage;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.Server;
import org.msgpack.util.codegen.DynamicPacker;
import org.msgpack.util.codegen.DynamicTemplate;

public class ServerTest extends TestCase {
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
            int ret = i + j;
        }
    }

    @Test
    public void testSyncLoad() throws Exception {
        CustomMessage.registerPacker(Foo.class, DynamicPacker
                .create(Foo.class));
        CustomMessage.registerTemplate(Foo.class, DynamicTemplate
                .create(Foo.class));
        
        EventLoop loop = new EventLoop();
        Server svr = new Server(loop);
        Client c = new Client("127.0.0.1", 19850);
        ITestHandler2 cc = (ITestHandler2) DynamicSyncClient.create(c,
                ITestHandler2.class);
        try {
            // svr.serve(new DynamicCodeGenDispatcher(new TestHandler()));
            try {
                svr.serve(new DynamicDispatcher(ITestHandler.class,
                        new TestHandler()));
            } catch (Throwable e) {
                e.printStackTrace();
            }
            svr.listen(19850);

            int num = 1000;

            long start = System.currentTimeMillis();
            for (int i = 0; i < num; i++) {
                MessagePackObject ret0 = c.callApply("m0", new Object[] { i,
                        i + 1 });
                assertEquals(2 * i + 1, ret0.intValue());
                MessagePackObject ret1 = c.callApply("m1", new Object[] { i,
                        i + 1 });
                assertEquals(2 * i + 1, ret1.intValue());
                MessagePackObject ret2 = c.callApply("m2", new Object[] { i,
                        i + 1 });
                assertEquals(2 * i + 1, ret2.intValue());
                Foo foo = new Foo();
                foo.i = i;
                foo.j = i + 1;
                MessagePackObject ret3 = c
                        .callApply("m3", new Object[] { foo });
                assertEquals(2 * i + 1, ret3.intValue());
                List<Integer> list = new ArrayList<Integer>();
                list.add(i);
                list.add(i + 1);
                MessagePackObject ret4 = c.callApply("m4",
                        new Object[] { list });
                List<MessagePackObject> ret40 = ret4.asList();
                assertEquals(2 * i + 1, ret40.get(0).intValue());
                int ret5 = cc.m0(i, i + 1);
                assertEquals(2 * i + 1, ret5);
                int ret6 = cc.m3(foo);
                assertEquals(2 * i + 1, ret6);
                List<Integer> ret7 = cc.m4(list);
                assertEquals(2 * i + 1, ret7.get(0).intValue());
                cc.m5(i, i + 1);
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
            CustomMessage.registerPacker(Foo.class, DynamicPacker
                    .create(Foo.class));
            CustomMessage.registerTemplate(Foo.class, DynamicTemplate
                    .create(Foo.class));
            svr.serve(new DynamicDispatcher(new TestHandler()));
            svr.listen(19850);

            int num = 100;

            long start = System.currentTimeMillis();
            for (int i = 0; i < num - 1; i++) {
                c.notifyApply("m0", new Object[] { i, i + 1 });
                c.notifyApply("m1", new Object[] { i, i + 1 });
                c.notifyApply("m2", new Object[] { i, i + 1 });
                Foo foo = new Foo();
                foo.i = i;
                foo.j = i + 1;
                c.notifyApply("m3", new Object[] { foo });
            }
            @SuppressWarnings("unused")
            MessagePackObject ret0 = c.callApply("m0", new Object[] { 0, 1 });
            long finish = System.currentTimeMillis();

            double result = num / ((double) (finish - start) / 1000);
            System.out.println("async: " + result + " calls per sec");

        } finally {
            svr.close();
            c.close();
            loop.shutdown();
        }
    }
}
