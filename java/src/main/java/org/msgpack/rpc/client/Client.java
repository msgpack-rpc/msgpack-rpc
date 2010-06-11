package org.msgpack.rpc.client;

/**
 * <p>
 * The MessagePack-RPC Client class. This class is an abstract class, because
 * it doesn't depend on any transport. The user actually touches the TCPClient
 * or UDPClient class.
 * </p>
 * <p>
 * The Client class supports two types of RPC call: the synchronous call, and
 * the asynchronous call.
 * </p>
 * <p>
 * The synchronous call waits for the completion of the request, while
 * asynchronous call doesn't. The asynchronous call is useful when calling
 * the remote function in parallel.
 * </p>
 * <p>
 * The following code is a sample to call a remote function synchronously.
 * </p>
 * <p><pre><code>
 * EventLoop loop = new EventLoop();
 * try {
 *   Client c = new TCPClient("localhost", 1985, loop);
 *   try {
 *     Object o;
 *     try {
 *       o = c.call("func", 1, 2);
 *     } catch (Exception e) {}
 *   } finally {
 *     c.close();
 *   }
 * } finally {
 *   loop.shutdown();
 * }
 * </code></pre></p>
 *
 * <p>
 * The following figure depicts how the synchronous call works.
 * The call() function returns after it receives the replies from the server.
 * </p>
 * <p><pre>
 * #     ---------- server
 * #    ^         |
 * #    |         |
 * # ---+         +----- client
 * #    call
 * </pre></p>
 * 
 * <p>
 * Otherwise, the code shown below is a sample of the asynchronous-call.
 * </p>
 * <p><pre><code>
 * EventLoop loop = new EventLoop();
 * try {
 *   Client c = new TCPClient("localhost", 1985, loop);
 *   try {
 *     try {
 *       // call two methods concurrently
 *       Future f1 = c.callAsync("func", 1, 2);
 *       Future f2 = c.callAsync("func", 1, 2);
 *     
 *       // join the results
 *       f1.join();
 *       f2.join();
 *       
 *       // get the results
 *       Object o1 = f.getResult();
 *       Object o2 = f.getResult();
 *     } catch (Exception e) {}
 *   } finally {
 *     c.close();
 *   }
 * } finally {
 *   loop.shutdown();
 * }
 * </code></pre></p>
 * 
 * <p>
 * The following figure depicts how the synchronous call works.
 * The callAsync() function returns immediately, and the client is able to
 * post the second request. The return type of call_async() is Future class.
 * Second, the clients calls join() function of the Future to wait the request
 * completion. Finally, calling getResult() function to get the actual result.
 * </p>
 * <p><pre>
 * #     ------------------ server
 * #    ^                 |
 * #    |        ---------|-------- server
 * #    |       ^         |       |
 * #    |       |         |       |
 * # ---+-------+-----    +-------+----- client
 * # call_a   call_a      join    join
 * </pre></p>
 */
public abstract class Client extends Session {
    public Client(String host, int port, EventLoop loop) {
        super(new Address(host, port), loop);
    }
    
    /**
     * Synchronous RPC call.
     * @param method the name of the method.
     * @param args the name of the method.
     * @return the result, returned by the server.
     * @throws Exception
     */
    public Object call(String method, Object... args) throws Exception {
        Future f = callAsync(method, args);
        f.join();
        return f.getResult();
    }

    /**
     * Asynchronous RPC call.
     * @param method the name of the method.
     * @param args the name of the method.
     * @return the Future class, which represents the returned result
     * by the server.
     * @throws Exception
     */
    public Future callAsync(String method, Object... args) throws Exception {
        if (args == null) {
            // This is a workaround for Java variable-length arguments.
            //
            // If we call this function like this...:
            //   send("method", null)
            // Then it's interpreted as:
            //   send("method", (Object[])null)
            // Therefore, 'args' becomes null. However, we want to be like:
            //   send("method", new Object[]{ null })
            //
            // For this reason, the type conversion is needed here.
            args = new Object[]{ null };
        }
        return sendRequest(method, args);
    }
    
    /**
     * Close the connection associated with this Client.
     */
    public void close() {
        super.tryClose();
    }
}
