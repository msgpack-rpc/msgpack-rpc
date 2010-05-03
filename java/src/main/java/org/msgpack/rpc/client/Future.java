package org.msgpack.rpc.client;

import java.io.IOException;

public class Future {
	protected Object error;
	protected Object result;
    protected boolean isJoined;
	
    public Future() {
        this.error = null;
        this.result = null;
        this.isJoined = false;
    }

    public synchronized void join() {
    	try {
    		while (error == null && result == null)
    			this.wait();
    	} catch (Exception e) {
    		e.printStackTrace();
    		error = e;
    	}
    	isJoined = true;
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

    public synchronized Object getResult() throws Exception {
    	if (!isJoined)
    		throw new IOException("Calling getResult() without join()");
        return result;
    }
    
    public synchronized Object getError() throws Exception {
       	if (!isJoined)
    		throw new IOException("Calling getResult() without join()");
    	return error;
    }
}
