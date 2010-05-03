package org.msgpack.rpc.client;

import java.io.IOException;

public class Future {
    protected Object error;
    protected Object result;

    public Future() {
        this.error = null;
        this.result = null;
    }

    public synchronized void join() {
    	try {
    		while (error == null && result == null)
    			this.wait();
    	} catch (Exception e) {
    		e.printStackTrace();
    		error = e;
    	}
    }
    
    public void setResult(Object result) {
    	set(null, result);
    }

    public void setError(Object error) {
    	if (error instanceof String)
    		error = new IOException((String)error);
    	set(error, null);
    }

    protected synchronized void set(Object error, Object result) {
    	this.error = error;
        this.result = result;
        this.notifyAll();
    }

    public synchronized Object getResult() {
        return result;
    }
}
