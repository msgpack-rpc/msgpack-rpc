package org.msgpack.rpc.client;

public class Client extends Session {
	protected EventLoop loop;
	
	public Client(String host, int port, EventLoop loop) {
		super(new Address(host, port), loop);
		this.loop = loop;
	}
	
	public Object call(String method, Object... args) throws Exception {
		Future f = sendRequest(method, args);
		f.join();
		return f.getResult();
	}

	public Future send(String method, Object... args) throws Exception {
		return sendRequest(method, args);
	}
	
	public void close() {
		super.tryClose();
	}
}
