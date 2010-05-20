package org.msgpack.rpc.client;

abstract public class Transport {
    abstract public void sendMessage(Object msg) throws Exception;
    abstract public void tryClose();
}
