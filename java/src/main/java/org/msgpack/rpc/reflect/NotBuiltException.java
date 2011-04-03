package org.msgpack.rpc.reflect;

@SuppressWarnings("serial")
public class NotBuiltException extends Exception {
    public NotBuiltException(Throwable t) {
        super(t);
    }

    public NotBuiltException(String reason, Throwable t) {
        super(reason, t);
    }
}
