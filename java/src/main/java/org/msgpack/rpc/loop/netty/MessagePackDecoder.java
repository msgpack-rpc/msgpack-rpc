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
package org.msgpack.rpc.loop.netty;

import java.nio.ByteBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.handler.codec.oneone.OneToOneDecoder;
import org.msgpack.MessagePack;
import org.msgpack.type.Value;

public class MessagePackDecoder extends OneToOneDecoder {

    MessagePack messagePack;

    public MessagePackDecoder(MessagePack messagePack) {
        super();
        this.messagePack = messagePack;
    }

    @Override
    protected Object decode(ChannelHandlerContext ctx, Channel channel,
            Object msg) throws Exception {
        if (!(msg instanceof ChannelBuffer)) {
            return msg;
        }

        ChannelBuffer source = (ChannelBuffer) msg;

        ByteBuffer buffer = source.toByteBuffer();
        if (!buffer.hasRemaining()) {
            return null;
        }

        byte[] bytes = buffer.array(); // FIXME buffer must has array
        int offset = buffer.arrayOffset() + buffer.position();
        int length = buffer.arrayOffset() + buffer.limit();

        Value v = messagePack.read(bytes, offset, length);
        return v;

        // TODO MessagePack.unpack()
        /*
         * Unpacker pac = new Unpacker(); pac.wrap(bytes, offset, length);
         * return pac.unpackObject();
         */
    }
}
