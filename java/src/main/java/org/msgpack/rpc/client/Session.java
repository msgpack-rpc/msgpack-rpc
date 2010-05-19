package org.msgpack.rpc.client;

import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.msgpack.rpc.Constants;

/**
 * Session handles send/recv request of the message, by using underlying
 * transport layer.
 *
 * this.reqTable stores the relationship between message id and corresponding
 * future. When the new requests are sent, the Session generates new message
 * id and new future. Then the Session registers them to req_table.
 * 
 * When it receives the message, the Session lookups the req_table and set the
 * result to the corresponding future.     
 */
public class Session {
    protected final Address addr;
    protected final EventLoop loop;

    protected double timeoutSec;
    protected HashMap<Integer, Future> reqTable;
    protected TCPTransport transport;
    
    static int msgidCounter = 0;

    public Session(Address addr, EventLoop loop) {
        this.addr = addr;
        this.loop = loop;
        this.timeoutSec = Constants.DEFAULT_TIMEOUT_SEC;

        this.reqTable = new HashMap<Integer, Future>();
        this.transport = null;
    }

    /**
     * Retrieve the address associated with this Session.
     * @return the address.
     */
    public Address getAddress() {
        return addr;
    }

    /**
     * Set the timeout value for this Session.
     * @param timeoutSec timeout value in seconds.
     */
    public synchronized void setTimeoutSec(double timeoutSec) {
        this.timeoutSec = timeoutSec;
    }

    /**
     * Get the timeout value for this Session.
     * @return timeout value in seconds.
     */
    public synchronized double getTimeoutSec() {
        return timeoutSec;
    }

    /**
     * Send the request to the remote MessagePack-RPC server. This takes
     * the following steps.                                                                                                                                                      
     * (1) Generates the new message id and the new future.
     * (2) Registers them to the reqTable.
     * (3) Passes the message to the underlying transport layer.
     * @param method method name to call
     * @param args arrays of the arguments
     * @return Future instance
     * @note future.join() will give you the result.
     */
    protected Future sendRequest(String method, Object[] args) {
        int msgid;
        Future future = new Future(getTimeoutSec());
        synchronized(this) {
            msgid = generateMessageID();
            reqTable.put(msgid, future);
        }
        try {
            ArrayList<Object> request = createRPCMessage(Constants.TYPE_REQUEST, msgid, method, args);
            getTransport().sendMessage(request);
        } catch (Exception e) {
            e.printStackTrace();
            future.setError(e);
        }
        return future;
    }
    
    /**
     * Create new transport when it's not available. If exists, return that.
     * @return transport class
     */
    private synchronized TCPTransport getTransport() {
        if (transport != null) return transport;
        transport = new TCPTransport(this, loop);
        return transport;
    }

    /**
     * Generate new message id, from the global counter.
     * @return generated message id
     */
    private int generateMessageID() {
        int msgid = Session.msgidCounter++;
        if (msgid > 1 << 30) Session.msgidCounter = 0;
        return msgid;
    }
    
    /**
     * Create tuples for MessagePack-RPC.
     * @param type the type.
     * @param msgid the message id.
     * @param method the name of the method.
     * @param args the arguments of the method.
     * @return the tuple for RPC.
     */
    private ArrayList<Object> createRPCMessage(int type, int msgid, String method, Object[] args) {
        ArrayList<Object> message = new ArrayList<Object>();
        message.add(Constants.TYPE_REQUEST);
        message.add(msgid);
        message.add(method);
        ArrayList<Object> params = new ArrayList<Object>();
        if (args != null) {
            for (Object o : args)
                params.add(o);
        }
        message.add(params);
        return message;
    }

    /**
     * Try to close this session.
     */
    protected synchronized void tryClose() {
        if (transport != null)
            transport.tryClose();
        transport = null;
    }

    /**
     * The callback called when the message arrives
     * @param replyObject the received object, already unpacked.
     * @throws Exception
     */
    public void onMessageReceived(Object replyObject) throws Exception {
        if (!(replyObject instanceof AbstractList))
            throw new IOException("invalid MPRPC Response"); // FIXME
        
        AbstractList a = (AbstractList)replyObject;
        if (a.size() != 4)
            throw new IOException("invalid MPRPC Protocol"); // FIXME

        Object objType   = a.get(0);
        Object objMsgID  = a.get(1);
        Object objError  = a.get(2);
        Object objResult = a.get(3);
        
        int msgid;
        if (objMsgID instanceof Number)
            msgid = ((Number)objMsgID).intValue();
        else
            throw new IOException("invalid msgid");
        
        Future future;
        synchronized (this) {
            if (!reqTable.containsKey(msgid))
                throw new IOException("not my msgid: msgid=" + msgid);
            future = reqTable.get(msgid);
            reqTable.remove(msgid);
        }

        try {
            int type = ((Number)objType).intValue();
            if (type != Constants.TYPE_RESPONSE) {
                future.setError("Invalid MPRPC Response Type");
                return;
            }
            if (objError != null) {
                String errorString = "Error";
                if (objError instanceof byte[])
                    errorString = new String((byte[])objError);
                future.setError(errorString);
                return;
            }
            future.setResult(objResult);
        } catch (Exception e) {
            future.setError(e);
        }
    }

    /**
     * The callback function, called when the connection failed.
     */
    public synchronized void onConnectFailed() {
        for (Entry<Integer, Future> ent : reqTable.entrySet()) {
            Future f = ent.getValue();
            f.setError("Connect Failed");
        }
        reqTable.clear();
        tryClose();
    }

    /**
     * The callback called when the connection closed.
     */
    public synchronized void onClosed() {
        for (Entry<Integer, Future> ent : reqTable.entrySet()) {
            Future f = ent.getValue();
            f.setError("Connection Closed");
        }
        reqTable.clear();
        tryClose();
    }

    /**
     * The callback called when the error occurred.
     * @param e occurred exception.
     */
    public synchronized void onFailed(Exception e) {
        for (Entry<Integer, Future> ent : reqTable.entrySet()) {
            Future f = ent.getValue();
            f.setError(e);
        }
        reqTable.clear();
        tryClose();
    }
}
