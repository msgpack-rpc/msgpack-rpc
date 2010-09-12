package org.msgpack.rpc.util.codegen;

public class DynamicCodegenException extends RuntimeException {

    public DynamicCodegenException(String reason) {
        super(reason);
    }

    public DynamicCodegenException(String reason, Throwable t) {
        super(reason, t);
    }
}
