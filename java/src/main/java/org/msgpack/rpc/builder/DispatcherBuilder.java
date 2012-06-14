package org.msgpack.rpc.builder;

import org.msgpack.MessagePack;
import org.msgpack.rpc.dispatcher.Dispatcher;

/**
 * User: takeshita
 * Create: 12/06/15 0:51
 */
public interface DispatcherBuilder {

    public Dispatcher build(Object handler,MessagePack messagePack) ;

}
