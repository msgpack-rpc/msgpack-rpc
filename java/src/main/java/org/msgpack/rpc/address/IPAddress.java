//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 FURUHASHI Sadayuki
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
package org.msgpack.rpc.address;

import java.net.SocketAddress;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.UnknownHostException;

public class IPAddress extends Address {
	private byte[] address;
	private int port;

	public IPAddress(String host, int port) throws UnknownHostException {
		this.address = InetAddress.getByName(host).getAddress();
		this.port = port;
	}

	public IPAddress(int port) {
		this.address = new InetSocketAddress(port).getAddress().getAddress();
		this.port = port;
	}

	public IPAddress(InetSocketAddress addr) {
		this.address = addr.getAddress().getAddress();
		this.port = addr.getPort();
	}

	public IPAddress(InetAddress addr, int port) throws UnknownHostException {
		this.address = addr.getAddress();
		this.port = port;
	}

	public InetSocketAddress getInetSocketAddress() {
		try {
			return new InetSocketAddress(InetAddress.getByAddress(address), port);
		} catch (UnknownHostException e) {
			throw new RuntimeException(e.getMessage());
		}
	}

	public SocketAddress getSocketAddress() {
		return getInetSocketAddress();
	}
}

