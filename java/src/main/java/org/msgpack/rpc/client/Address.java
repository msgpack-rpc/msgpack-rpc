package org.msgpack.rpc.client;

public class Address {
	protected String host;
	protected int port;
	
	Address(String host, int port) {
		this.host = host;
		this.port = port;
	}
	
	String getHost() {
		return host;
	}
	
	int getPort() {
		return port;
	}
}
