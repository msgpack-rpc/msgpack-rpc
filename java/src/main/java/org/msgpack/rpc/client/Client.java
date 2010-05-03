package org.msgpack.rpc.client;

public class Client extends Session {
	protected EventLoop loop;
	
	public Client(String host, int port) {
		this(host, port, new EventLoop());
	}

	public Client(String host, int port, EventLoop loop) {
		super(new Address(host, port), loop);
		this.loop = loop;
	}
	
	public void close() {
		super.tryClose();
		loop.shutdown();
	}
}
