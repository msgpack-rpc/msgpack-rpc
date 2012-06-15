package org.msgpack.rpc;

import junit.framework.TestCase;
import org.apache.log4j.BasicConfigurator;
import org.junit.Test;
import org.msgpack.MessageTypeException;
import org.msgpack.rpc.builder.StopWatchDispatcherBuilder;
import org.msgpack.rpc.error.NoMethodError;
import org.msgpack.rpc.error.RemoteError;
import org.msgpack.rpc.error.TimeoutError;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.type.Value;

import java.util.Date;

/**
 * Test when server throws exceptions.
 * User: takeshita
 * Create: 12/06/15 12:12
 */
public class ServerErrorTest  extends TestCase {


    public static class TestServer{

        public String echo(String message){
            return message;
        }


        public String waitWhile(int waitMSecs) throws Exception{
            Thread.sleep(waitMSecs);
            return "ok";
        }

        public String throwException(String errorMessage) throws Exception{
            throw new MessageTypeException(errorMessage);
        }

        public String throwRuntimeException(String errorMessage) {
            throw new RuntimeException(errorMessage);
        }

    }

    static interface CallFunc{
        public void apply(Client client);
    }

    void call( CallFunc func)  throws Exception{

        BasicConfigurator.configure();
        EventLoop loop = EventLoop.start();
        Server svr = new Server(loop);
        Client c = new Client("127.0.0.1", 19850, loop);
        c.setRequestTimeout(1);

        try {
            svr.serve(new TestServer());
            svr.listen(19850);

            func.apply(c);

        } finally {
            svr.close();
            c.close();
            loop.shutdown();
        }
    }


    @Test
    public void testNormalException()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                String message = "Normal exception";
                try{
                    client.callApply("throwException",new Object[]{message});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    assertEquals(message,e.getMessage());
                }catch(Exception e){
                    System.out.println(e.getClass());
                    fail("Not normal exception");
                }

            }
        }
        );

    }

    @Test
    public void testRuntimeException()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                String message = "Normal exception";
                try{
                    client.callApply("throwRuntimeException",new Object[]{message});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    assertEquals(message,e.getMessage());
                }catch(Exception e){
                    System.out.println(e.getClass());
                    fail("Not normal exception");
                }

            }
        }
        );

    }
    @Test
    public void testNullErrorMessage()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                try{
                    client.callApply("throwRuntimeException",new Object[]{null});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    assertEquals("",e.getMessage());
                }catch(Exception e){
                    System.out.println(e.getClass());
                    fail("Not normal exception");
                }

            }
        }
        );

    }

    @Test
    public void testNoMethodError()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                try{
                    client.callApply("methodWhichDoesNotExist",new Object[]{null});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    assertEquals(".CallError.NoMethodError",e.getMessage());
                }catch(Exception e){
                    System.out.println("Wrong exception:" + e.getClass());
                    fail("Not NoMethodException");
                }

            }
        }
        );

    }
    @Test
    public void testBadArgs()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                try{
                    client.callApply("echo",new Object[]{1});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    // OK
                }catch(Exception e){
                    System.out.println("Wrong exception:" + e.getClass());
                    fail("Not NoMethodException");
                }

            }
        }
        );

    }
    @Test
    public void testTimeout()  throws Exception {
        call( new CallFunc(){
            public void apply(Client client) {
                try{
                    client.callApply("waitWhile",new Object[]{3000});
                    fail("Must throw exception");
                }catch(RemoteError e){
                    assertEquals("timedout",e.getMessage());
                }catch(Exception e){
                    System.out.println("Wrong exception:" + e.getClass());
                    fail("Not NoMethodException");
                }

            }
        }
        );

    }
}
