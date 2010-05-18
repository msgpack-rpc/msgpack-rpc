package org.msgpack.rpc.client;

import java.io.IOException;

public class Future {
    protected Exception error;
    protected Object result;
    protected boolean isSet;
    protected boolean isJoined;
    protected long expireMills;
    protected boolean isTimeouted;

    public Future(double timeoutSec) {
        this.error = null;
        this.result = null;
        this.isSet = false;
        this.isJoined = false;
        this.expireMills = System.currentTimeMillis() + (long)(timeoutSec * 1000);
        this.isTimeouted = false;
    }

    public synchronized void join() {
        try {
            while (true) {
                System.out.println("joining...");
                if (isSet)
                    break;
                
                // check if timeout occurred
                long curMills = System.currentTimeMillis();
                if (curMills >= expireMills) {
                    isTimeouted = true;
                    throw new IOException("timeout occurred");
                }
                
                this.wait(expireMills - curMills);
            }
        } catch (Exception e) {
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
        if (isTimeouted) return;
        this.isSet = true;
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
