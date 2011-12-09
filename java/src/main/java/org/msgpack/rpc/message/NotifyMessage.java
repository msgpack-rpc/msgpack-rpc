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
package org.msgpack.rpc.message;

import java.io.IOException;
import org.msgpack.MessagePackable;
import org.msgpack.packer.Packer;
import org.msgpack.type.Value;
import org.msgpack.MessageTypeException;
import org.msgpack.unpacker.Unpacker;

public class NotifyMessage implements MessagePackable {
    private String method;
    private Object[] args;

    public NotifyMessage(String method, Object[] args) {
        this.method = method;
        this.args = args;
    }

    // public String getMethodName() {
    // return method;
    // }

    // public Object getArguments() {
    // return args;
    // }

    public void writeTo(Packer pk) throws IOException {
        pk.writeArrayBegin(3);
        pk.write(Messages.NOTIFY);
        pk.write(method);
        pk.write(args.length);
        for (Object arg : args) {
            pk.write(arg);
        }
        pk.writeArrayEnd();
    }

    public void readFrom(Unpacker u) throws IOException {
        throw new UnsupportedOperationException();
    }

    public void messagePack(Packer pk) throws IOException {
        writeTo(pk);
        /*
         * pk.packArray(3); pk.packInt(Messages.NOTIFY); pk.packString(method);
         * pk.packArray(args.length); for(Object arg : args) { pk.pack(arg); }
         */
    }

    // FIXME messageConvert
}
