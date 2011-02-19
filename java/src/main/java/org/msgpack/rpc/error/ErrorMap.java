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

public class ErrorMap {
	// FIXME TreeMap<String, Class<RPCError>> map;
	// "RemoteError" => RemoteError.class
	// static private TreeMap<String, Class<RPCError>> systemMap;

	public ErrorMap() {
	}

	public void register(String code, Class<RPCError> klass) {
		// FIXME
	}

	public void raise(String code, MessagePackObject data) throws RPCError {
		// FIXME
	}

	/*
	static private ErrorMap defaultErrorMap;

	static public ErrorMap defaultErrorMap() {
		// FIXME
	}
	*/
}

