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
package org.msgpack.rpc.transport;

import java.io.*;
import java.util.*;
import java.net.*;
import org.msgpack.*;
import org.msgpack.rpc.*;
import org.jboss.xnio.*;
import org.jboss.xnio.channels.*;

class TCPClientTransportPeer implements MessageSendable {
	private Object lock = new Object();
	private List<TCPClientSocket> sockpool = new ArrayList<TCPClientSocket>();
	private int connecting = 0;
	private VectorOutputStream pending = new VectorOutputStream();

	private Session session;
	private double connectTimeout;
	private int reconnectLimit;

	TCPClientTransportPeer(Session session, TCPClientTransport transport) {
		this.session = session;
		this.connectTimeout = transport.getConnectTimeout();
		this.reconnectLimit = transport.getReconnectLimit();
	}

	public void close() {
		synchronized(lock) {
			pending.reset();
			for(TCPClientSocket sock : sockpool) {
				sock.close();
			}
			sockpool.clear();
			connecting = -1;
		}
	}

	Session getSession() {
		return session;
	}

	public void sendMessage(Object msg) {
		synchronized(lock) {
			if(connecting == -1) { return; }  // already closed
			if(sockpool.isEmpty()) {
				if(connecting == 0) {
					tryConnect(lock);
					connecting++;
				}
				try {
					new Packer(pending).pack(msg);
				} catch (IOException e) {
					// FIXME
				}
			} else {
				// FIXME sudo connecting load balance
				TCPClientSocket sock = sockpool.get(0);
				sock.sendMessage(msg);
			}
		}
	}

	private static final IoFuture.Notifier<TcpChannel,TCPClientTransportPeer> connectNotifier =
		new IoFuture.Notifier<TcpChannel, TCPClientTransportPeer>() {
			public void notify(IoFuture<? extends TcpChannel> future, TCPClientTransportPeer attachment) {
				try {
					TcpChannel channel = future.get();
					attachment.onConnectSucess(channel);
				} catch(IOException e) {
					attachment.onConnectFailed(e);
				}
			}
		};

	private void tryConnect(Object locked) {
		InetSocketAddress addr = (InetSocketAddress)session.getSocketAddress();
		OptionMap opts = OptionMap.EMPTY;  // FIXME
		TcpConnector connector = session.getEventLoop().getXnio().createTcpConnector(opts);
		IoFuture<TcpChannel> future = connector.connectTo(addr, null, null);
		future.addNotifier(connectNotifier, this);
	}

	void onConnectSucess(TcpChannel channel) {
		TCPClientSocket sock = new TCPClientSocket(channel, this);
		synchronized(lock) {
			if(connecting == -1) { sock.close(); return; }  // already closed
			sockpool.add(sock);
			if(!pending.isEmpty()) {
				sock.sendPending(pending);
				pending.reset();
			}
			connecting = 0;
		}
	}

	void onConnectFailed(IOException e) {
		synchronized(lock) {
			if(connecting == -1) { return; }  // already closed
			if(connecting < reconnectLimit) {
				tryConnect(lock);
				connecting++;
			} else {
				connecting = 0;
				pending.reset();
				session.transportConnectFailed();
			}
		}
	}

	void removeSocket(TCPClientSocket sock) {
		synchronized(lock) {
			if(connecting == -1) { return; }  // already closed
			sockpool.remove(sock);
		}
	}
}

