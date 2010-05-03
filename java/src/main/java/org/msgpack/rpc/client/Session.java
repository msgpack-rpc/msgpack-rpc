package org.msgpack.rpc.client;

import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.msgpack.rpc.Constants;

public class Session {
	protected Address addr;
	protected EventLoop loop;
	protected HashMap<Integer, Future> reqTable;
	protected TCPTransport transport;
	
	static int msgidCounter = 0;
	
	public Session(Address addr, EventLoop loop) {
		this.addr = addr;
		this.loop = loop;
		this.reqTable = new HashMap<Integer, Future>();
		this.transport = null;
	}
	
	public Address getAddress() {
		return addr;
	}

	public Object call(String method, Object... args) throws Exception {
		Future f = sendRequest(method, args);
		f.join();
		return f.getResult();
	}

	public Future send(String method, Object... args) throws Exception {
		return sendRequest(method, args);
	}

	public synchronized void close() {
		if (transport != null)
			transport.close();
		transport = null;
	}

	protected synchronized TCPTransport getTransport() {
		if (transport != null) return transport;
		transport = new TCPTransport(this, loop);
		return transport;
	}

	protected Future sendRequest(String method, Object[] args) {
		int msgid;
		Future future = new Future();
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
	
	protected int generateMessageID() {
		int msgid = Session.msgidCounter++;
		if (msgid > 1 << 30) Session.msgidCounter = 0;
		return msgid;
	}
	
	protected ArrayList<Object> createRPCMessage(int type, int msgid, String method, Object[] args) {
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

	// callback
	public synchronized void onMessageReceived(Object replyObject) throws Exception {
		if (!(replyObject instanceof AbstractList))
			throw new IOException("invalid MPRPC Response"); // FIXME
		
        AbstractList a = (AbstractList)replyObject;
        if (a.size() != 4)
        	throw new IOException("invalid MPRPC Protocol"); // FIXME

        Object objType   = a.get(0);
        Object objMsgID  = a.get(1);
        Object objError  = a.get(2);
        Object objResult = a.get(3);
        if (!(objMsgID instanceof Number))
        	throw new IOException("invalid msgid"); // FIXME
        int msgid = ((Number)objMsgID).intValue();
        if (!reqTable.containsKey(msgid))
        	throw new IOException("not my msgid: msgid=" + msgid);
        
        Future future = reqTable.get(msgid);
        reqTable.remove(msgid);
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
        	future.setError(e.getMessage());
        }
	}
	
	// callback
	public synchronized void onConnectFailed() {
		for (Entry<Integer, Future> e : reqTable.entrySet()) {
			Future f = e.getValue();
			f.setError("Connect Failed");
		}
		reqTable.clear();
		close();
	}

	// callback
	public synchronized void onClosed() {
		for (Entry<Integer, Future> e : reqTable.entrySet()) {
			Future f = e.getValue();
			f.setError("Connection Closed");
		}
		reqTable.clear();
		close();
	}

	// callback
	public synchronized void onFailed() {
		for (Entry<Integer, Future> e : reqTable.entrySet()) {
			Future f = e.getValue();
			f.setError("Failed");
		}
		reqTable.clear();
		close();
	}
}
