package org.msgpack.rpc.server;

import java.io.IOException;

public abstract class Server {
    public abstract void serv() throws IOException;
    public abstract void stop();
}
