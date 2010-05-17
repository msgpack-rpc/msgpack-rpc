package org.msgpack.rpc.client;

import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.msgpack.rpc.Constants;

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
	
	public Address getAddress() {
		return addr;
	}
	
	public synchronized void setTimeoutSec(double timeoutSec) {
		this.timeoutSec = timeoutSec;
	}
	
	public synchronized double getTimeoutSec() {
		return timeoutSec;
	}

	protected synchronized TCPTransport getTransport() {
		if (transport != null) return transport;
		transport = new TCPTransport(this, loop);
		return transport;
	}

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
	
	private int generateMessageID() {
		int msgid = Session.msgidCounter++;
		if (msgid > 1 << 30) Session.msgidCounter = 0;
		return msgid;
	}
	
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
	
	protected synchronized void tryClose() {
		if (transport != null)
			transport.tryClose();
		transport = null;
	}

	// callback
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
	
	// callback
	public synchronized void onConnectFailed() {
		for (Entry<Integer, Future> ent : reqTable.entrySet()) {
			Future f = ent.getValue();
			f.setError("Connect Failed");
		}
		reqTable.clear();
		tryClose();
	}

	// callback
	public synchronized void onClosed() {
		for (Entry<Integer, Future> ent : reqTable.entrySet()) {
			Future f = ent.getValue();
			f.setError("Connection Closed");
		}
		reqTable.clear();
		tryClose();
	}

	// callback
	public synchronized void onFailed(Exception e) {
		for (Entry<Integer, Future> ent : reqTable.entrySet()) {
			Future f = ent.getValue();
			f.setError(e);
		}
		reqTable.clear();
		tryClose();
	}
}
