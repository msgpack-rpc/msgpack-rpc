package org.msgpack.rpc.dispatcher;

import org.msgpack.rpc.Request;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * For debug.
 * Measures dispatch time.
 *
 * User: takeshita
 * Create: 12/06/15 0:53
 */
public class StopWatchDispatcher implements Dispatcher {

    Dispatcher innerDispatcher;

    private final static Logger logger = LoggerFactory.getLogger(StopWatchDispatcher.class);

    private boolean verbose = true;

    public boolean isVerbose() {
        return verbose;
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }

    public StopWatchDispatcher(Dispatcher inner) {
        this.innerDispatcher = inner;
    }

    public void dispatch(Request request) throws Exception {
        if(verbose){
            logger.info(String.format( "Begin dispatching %s with args %s",request.getMethodName(),request.getArguments().toString()));
        }
        long start = System.currentTimeMillis();
        try{
            innerDispatcher.dispatch(request);
            long diff = System.currentTimeMillis() - start;
            logger.info(String.format("Dispatch %s in %s msecs",request.getMethodName(),diff));
        }catch(Exception e){
            long diff = System.currentTimeMillis() - start;
            logger.info(String.format("%s : %s while dispatching %s,(in %s msecs)",e.getClass().getSimpleName(), e.getMessage(), request.getMethodName(),diff));
            throw e;
        }
    }
}
