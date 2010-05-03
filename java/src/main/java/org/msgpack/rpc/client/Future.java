package org.msgpack.rpc.client;

import java.io.IOException;

public class Future {
	protected Exception error;
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
    	Exception e;
    	if (error instanceof String)
    		e = new IOException((String)error);
    	else if (error instanceof Exception)
    		e = (Exception)error;
    	else
    		e = new IOException("Unknown Error");
    	set(e, null);
    }

    protected synchronized void set(Exception error, Object result) {
    	this.error = error;
        this.result = result;
        this.notifyAll();
    }

    public synchronized Object getResult() throws Exception {
    	if (!isJoined)
    		throw new IOException("Calling getResult() without join()");
    	if (error != null)
    		throw error;
        return result;
    }
}
