package org.msgpack.rpc.builder;

import org.msgpack.MessagePack;
import org.msgpack.rpc.dispatcher.Dispatcher;
import org.msgpack.rpc.dispatcher.StopWatchDispatcher;

/**
 * User: takeshita
 * Create: 12/06/15 1:23
 */
public class StopWatchDispatcherBuilder implements DispatcherBuilder {

    protected DispatcherBuilder baseBuilder;
    protected boolean verbose = false;

    public boolean isVerbose() {
        return verbose;
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    public StopWatchDispatcherBuilder withVerboseOutput(boolean verbose){
        this.verbose = verbose;
        return this;
    }

    public StopWatchDispatcherBuilder(DispatcherBuilder baseBuilder){
        this.baseBuilder = baseBuilder;
    }

    public Dispatcher decorate(Dispatcher innerDispatcher) {
        StopWatchDispatcher disp = new StopWatchDispatcher(innerDispatcher);
        disp.setVerbose(verbose);
        return disp;
    }

    public Dispatcher build(Object handler, MessagePack messagePack) {
        return decorate(baseBuilder.build(handler,messagePack));
    }
}
