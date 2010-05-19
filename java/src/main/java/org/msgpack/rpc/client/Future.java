package org.msgpack.rpc.client;

import java.io.IOException;

/**
 * This class is used as the result of asynchronous call.
 * By using join(), the caller is able to wait for the completion.
 */
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

    /**
     * Wait for the request completion.
     */
    public synchronized void join() {
        try {
            while (true) {
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

    /**
     * Set the result to this future.
     * @param result the result object.
     */
    public void setResult(Object result) {
        set(null, result);
    }

    /**
     * Set the error to this future.
     * @param error the error message or exception.
     */
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

    /**
     * Set the error and result to this future.
     * @param error the exception object.
     * @param result the result object.
     */
    protected synchronized void set(Exception error, Object result) {
        if (isTimeouted) return;
        this.isSet = true;
        this.error = error;
        this.result = result;
        this.notifyAll();
    }

    /**
     * Try to get the result of this future. If the error happened, then the
     * exception is thrown.
     * @return the RPC result object.
     * @throws Exception
     */
    public synchronized Object getResult() throws Exception {
        if (!isJoined)
            throw new IOException("Calling getResult() without join()");
        if (error != null)
            throw error;
        return result;
    }
}
