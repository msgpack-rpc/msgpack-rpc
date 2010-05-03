package org.msgpack.rpc.client;

public class Client extends Session {
	public Client(String host, int port, EventLoop loop) {
		super(new Address(host, port), loop);
	}
}
