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
package org.msgpack.rpc.error;

import org.msgpack.*;
import org.msgpack.object.*;
import java.io.IOException;

public class RemoteError extends RPCError implements MessagePackable {
	private MessagePackObject data;

	public RemoteError() {
		super();
		this.data = ArrayType.create(new MessagePackObject[]{
			RawType.create("unknown error")
		});
	}

	public RemoteError(String message) {
		super(message);
		this.data = ArrayType.create(new MessagePackObject[]{
			RawType.create(message)
		});
	}

	public RemoteError(MessagePackObject data) {
		super(loadMessage(data));
		this.data = data;
	}

	public MessagePackObject getData() {
		return data;
	}

	public void messagePack(Packer pk) throws IOException {
		pk.pack(data);
	}

	private static String loadMessage(MessagePackObject data) {
		try {
			if(data.isRawType()) {
				return data.asString();
			} else {
				return data.asArray()[0].asString();
			}
		} catch (MessageTypeException e) {
			return "unknown error: "+data;
		}
	}

	public static final String CODE = "RemoteError";

	@Override
	public String getCode() {
		return CODE;
	}
}

