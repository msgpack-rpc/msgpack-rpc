//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 Kazuki Ohta
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//        http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//
package org.msgpack.rpc.client;

import java.io.IOException;

/**
 * This class is used as the result of asynchronous call.
 * By using join(), the caller is able to wait for the completion.
 */
public class Future {
    protected Exception error;
    protected Object result;
    protected boolean isFinished;
    protected boolean isJoined;
    protected long expireMills;
    protected boolean isTimeouted;

    public Future(double timeoutSec) {
        this.error = null;
        this.result = null;
        this.isFinished = false;
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
                if (isFinished)
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
            setError(e);
        }
        isJoined = true;
    }
    
    public synchronized boolean isFinished() {
        return isFinished;
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
        this.isFinished = true;
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
