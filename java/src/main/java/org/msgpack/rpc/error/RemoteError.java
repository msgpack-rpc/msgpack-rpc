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
import org.msgpack.packer.Packer;
import org.msgpack.type.Value;
import org.msgpack.type.ValueFactory;
import org.msgpack.unpacker.Unpacker;

import java.io.IOException;

public class RemoteError extends RPCError implements MessagePackable {
    private static final long serialVersionUID = 1L;

    private Value data;

    public RemoteError() {
        super();
        this.data = ValueFactory.createArrayValue(
                new Value[] { ValueFactory.createRawValue("unknown error") });
    }

    public RemoteError(String message) {
        super(message);
        this.data = ValueFactory.createArrayValue(
                new Value[] { ValueFactory.createRawValue(message) });
    }

    public RemoteError(Value data) {
        super(loadMessage(data));
        this.data = data;
    }

    public Value getData() {
        return data;
    }

    public void messagePack(Packer pk) throws IOException {
        pk.write(data);
    }

    private static String loadMessage(Value data) {
        try {
            if (data.isRawValue()) {
                return data.asRawValue().getString();
            } else {
                return data.asArrayValue().getElementArray()[0].asRawValue().getString();
            }
        } catch (MessageTypeException e) {
            return "unknown error: " + data;
        }
    }

    public void writeTo(Packer pk) throws IOException {
        pk.write(data);
    }

    public void readFrom(Unpacker u) throws IOException {
        data = u.readValue();
    }

    public static final String CODE = "RemoteError";

    @Override
    public String getCode() {
        return CODE;
    }
}
