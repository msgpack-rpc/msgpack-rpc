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

import java.io.IOException;
import org.msgpack.*;

public class ResponseMessage implements MessagePackable {
	private int msgid;
	private Object error;
	private Object result;

	public ResponseMessage(int msgid, Object error, Object result) {
		this.msgid = msgid;
		this.error = error;
		this.result = result;
	}

	public int getMessageID() {
		return msgid;
	}

	public Object getError() {
		return error;
	}

	public Object getResult() {
		return result;
	}

	@Override
	public void messagePack(Packer pk) throws IOException {
		pk.packArray(4);
		pk.packInt(1);  // FIXME Messages.RESPONSE
		pk.packInt(msgid);
		pk.pack(error);
		pk.pack(result);
	}

	// FIXME messageConvert
}

