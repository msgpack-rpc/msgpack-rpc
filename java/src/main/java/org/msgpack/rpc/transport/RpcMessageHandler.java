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

import org.msgpack.MessagePackObject;
import org.msgpack.rpc.message.Messages;
import org.msgpack.rpc.Session;
import org.msgpack.rpc.Server;

public class RpcMessageHandler {
	protected final Session session;
	protected final Server server;

	public RpcMessageHandler(Session session) {
		this(session, null);
	}

	public RpcMessageHandler(Server server) {
		this(null, server);
	}

	public RpcMessageHandler(Session session, Server server) {
		this.session = session;
		this.server = server;
	}

	public void handleMessage(MessageSendable channel, MessagePackObject msg) {
		MessagePackObject[] array = msg.asArray();

		// TODO check array.length
		int type = array[0].asInt();
		if(type == Messages.REQUEST) {
			// REQUEST
			int msgid = array[1].asInt();
			String method = array[2].asString();
			MessagePackObject args = array[3];
			handleRequest(channel, msgid, method, args);

		} else if(type == Messages.RESPONSE) {
			// RESPONSE
			int msgid = array[1].asInt();
			MessagePackObject error = array[2];
			MessagePackObject result = array[3];
			handleResponse(channel, msgid, result, error);

		} else if(type == Messages.NOTIFY) {
			// NOTIFY
			String method = array[1].asString();
			MessagePackObject args = array[2];
			handleNotify(channel, method, args);

		} else {
			// FIXME error result
			throw new RuntimeException("unknown message type: "+type);
		}
	}

	public void handleRequest(MessageSendable channel,
			int msgid, String method, MessagePackObject args) {
		if(server == null) {
			return;  // FIXME error result
		}
		server.onRequest(channel, msgid, method, args);
	}

	public void handleNotify(MessageSendable channel,
			String method, MessagePackObject args) {
		if(server == null) {
			return;  // FIXME error result?
		}
		server.onNotify(method, args);
	}

	public void handleResponse(MessageSendable channel,
			int msgid, MessagePackObject result, MessagePackObject error) {
		if(session == null) {
			return;  // FIXME error?
		}
		session.onResponse(msgid, result, error);
	}
}

