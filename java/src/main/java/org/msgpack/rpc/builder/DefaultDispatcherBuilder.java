package org.msgpack.rpc.builder;

import org.msgpack.MessagePack;
import org.msgpack.rpc.dispatcher.Dispatcher;
import org.msgpack.rpc.dispatcher.MethodDispatcher;
import org.msgpack.rpc.reflect.Reflect;

/**
 * User: takeshita
 * Create: 12/06/15 1:16
 */
public class DefaultDispatcherBuilder implements DispatcherBuilder {

    public Dispatcher build(Object handler, MessagePack messagePack) {
        return new MethodDispatcher(
                new Reflect(messagePack), handler);
    }
}

