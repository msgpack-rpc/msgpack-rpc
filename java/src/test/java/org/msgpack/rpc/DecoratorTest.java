package org.msgpack.rpc;

import junit.framework.TestCase;
import org.apache.log4j.BasicConfigurator;
import org.junit.Test;
import org.msgpack.MessageTypeException;
import org.msgpack.rpc.builder.StopWatchDispatcherBuilder;
import org.msgpack.rpc.loop.EventLoop;
import org.msgpack.type.Value;

/**
 * User: takeshita
 * Create: 12/06/15 1:51
 */
public class DecoratorTest extends TestCase {

    public static class TestServer{

        public String success() {
            return "ok";
        }

        public String throwError(String errorMessage) throws Exception{
            throw new MessageTypeException(errorMessage);
        }
    }


    /**
     * Test any Exception is not thrown.
     * @throws Exception
     */
    @Test
    public void testDecorateStopWatch()  throws Exception {

        BasicConfigurator.configure();
        EventLoop loop = EventLoop.start();
        Server svr = new Server(loop);
        svr.setDispatcherBuilder(
                new StopWatchDispatcherBuilder(svr.getDispatcherBuilder()).
                        withVerboseOutput(true)
        );
        Client c = new Client("127.0.0.1", 19850, loop);
        c.setRequestTimeout(10);

        try {
            svr.serve(new TestServer());
            svr.listen(19850);

            Value result = c.callApply("success", new Object[]{});

            assertNotNull(result);

            try{
                c.callApply("throwError", new Object[]{"StopWatchTest"});
                fail("Exception must be thrown.");
            }catch(Exception e){

            }

        } finally {
            svr.close();
            c.close();
            loop.shutdown();
        }
    }
}
