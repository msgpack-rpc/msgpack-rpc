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
package org.msgpack.rpc;

import java.io.*;
import java.util.*;
import org.msgpack.rpc.transport.*;
import org.msgpack.*;

public class Request {
	private MessageSendable ms;  // synchronized?
	private int msgid;
	private String method;
	private MessagePackObject args;

	public Request(MessageSendable ms, int msgid,
			String method, MessagePackObject args) {
		this.ms = ms;
		this.msgid = msgid;
		this.method = method;
		this.args = args;
	}

	public Request(String method, MessagePackObject args) {
		this.ms = null;
		this.msgid = 0;
		this.method = method;
		this.args = args;
	}

	public String getMethodName() {
		return method;
	}

	public MessagePackObject getArguments() {
		return args;
	}

	public int getMessageID() {
		return msgid;
	}

	public void sendResult(Object result) {
		sendResponse(result, null);
	}

	public void sendError(Object error) {
		sendResponse(null, error);
	}

	public void sendError(Object error, Object data) {
		sendResponse(data, error);
	}

	public synchronized void sendResponse(Object result, Object error) {
		if(ms == null) {
			return;
		}
		ResponseMessage msg = new ResponseMessage(msgid, error, result);
		ms.sendMessage(msg);
		ms = null;
	}
}

