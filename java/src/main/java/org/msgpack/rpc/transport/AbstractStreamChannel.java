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
import java.net.*;
import java.util.*;
import java.nio.*;
import org.msgpack.*;
import org.msgpack.rpc.*;
import org.jboss.xnio.*;
import org.jboss.xnio.channels.*;

abstract class AbstractStreamChannel implements MessageSendable {
	protected Unpacker pac = new Unpacker();
	protected ConnectedStreamChannel<InetSocketAddress> channel;

	public AbstractStreamChannel(ConnectedStreamChannel<InetSocketAddress> channel) {
		this.channel = channel;
		channel.getReadSetter().set(new ChannelListener<ConnectedStreamChannel<InetSocketAddress>>() {
				public void handleEvent(ConnectedStreamChannel<InetSocketAddress> channel) {
					onRead();
				}
			});
		channel.getWriteSetter().set(new ChannelListener<ConnectedStreamChannel<InetSocketAddress>>() {
				public void handleEvent(ConnectedStreamChannel<InetSocketAddress> channel) {
					onWrite();
				}
			});
		channel.getCloseSetter().set(new ChannelListener<ConnectedStreamChannel<InetSocketAddress>>() {
				public void handleEvent(ConnectedStreamChannel<InetSocketAddress> channel) {
					onClose();
					close();
				}
			});
		channel.resumeReads();
	}

	public void close() {
		try {
			channel.close();
		} catch (IOException e) {
			// FIXME exception
		}
	}

	private VectorOutputStream buffer = new VectorOutputStream();

	public synchronized void migratePending(VectorOutputStream from) {
		synchronized(buffer) {
			boolean empty = buffer.isEmpty();
			if(empty) {
				from.swap(buffer);
			} else {
				from.migrate(buffer);
			}
			if(empty) {
				onWrite();
			}
		}
	}

	public void sendMessage(Object msg) {
		synchronized(buffer) {
			boolean empty = buffer.isEmpty();
			try {
				new Packer(buffer).pack(msg);
			} catch (IOException e) {
			}
			if(empty) {
				onWrite();
			}
		}
	}

	void onWrite() {
		synchronized(buffer) {
			try {
				buffer.writeTo(channel);
			} catch (IOException e) {
				// FIXME exception
			}
			if(!buffer.isEmpty()) {
				channel.resumeWrites();
			}
		}
	}

	void onRead() {
		try {
			pac.reserveBuffer(32*1024);  // FIXME buffer size
			ByteBuffer out = ByteBuffer.wrap(pac.getBuffer(), pac.getBufferOffset(), pac.getBufferCapacity());
			int count = channel.read(out);

			if(count <= 0) {
				// FIXME log
				try {
					channel.close();
				} catch (IOException e) {
					// ignore
				}
				return;
			}

			pac.bufferConsumed(count);

			while(pac.execute()) {
				MessagePackObject msg = pac.getData();
				onMessage(msg);
				pac.reset();
			}

			channel.resumeReads();

		} catch(IOException e) {
			// FIXME exception
			try {
				channel.close();
			} catch (IOException ex) {
				// ignore
			}
			return;
		}
	}

	public void onMessage(MessagePackObject msg) {
		MessagePackObject[] array = msg.asArray();
		int type = array[0].asInt();
		if(type == 0) {
			// REQUEST
			int msgid = array[1].asInt();
			String method = array[2].asString();
			MessagePackObject args = array[3];
			onRequest(msgid, method, args);

		} else if(type == 1) {
			// RESPONSE
			int msgid = array[1].asInt();
			MessagePackObject error = array[2];
			MessagePackObject result = array[3];
			onResponse(msgid, error, result);

		} else if(type == 2) {
			// NOTIFY
			String method = array[1].asString();
			MessagePackObject args = array[2];
			onNotify(method, args);

		} else {
			// FIXME error result
			throw new RuntimeException("unknown message type: "+type);
		}
	}

	abstract void onClose();

	abstract public void onRequest(int msgid, String method, MessagePackObject args);

	abstract public void onNotify(String method, MessagePackObject args);

	abstract public void onResponse(int msgid, MessagePackObject error, MessagePackObject result);
}

