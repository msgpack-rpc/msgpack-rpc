package org.msgpack.rpc.client;

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
        Future f = send(method, args);
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
    public Future send(String method, Object... args) throws Exception {
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
