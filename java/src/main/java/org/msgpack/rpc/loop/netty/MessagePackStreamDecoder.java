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

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.nio.ByteBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.handler.codec.frame.FrameDecoder;
import org.msgpack.MessagePack;
import org.msgpack.type.Value;
import org.msgpack.unpacker.Unpacker;

public class MessagePackStreamDecoder extends FrameDecoder {
    protected MessagePack messagePack;

    public MessagePackStreamDecoder(MessagePack messagePack) {
        super();
        this.messagePack = messagePack;
    }

    @Override
    protected Object decode(ChannelHandlerContext ctx, Channel channel,
            ChannelBuffer source) throws Exception {
        ByteBuffer buffer = source.toByteBuffer();
        if (!buffer.hasRemaining()) {
            return null;
        }
        source.markReaderIndex();

        byte[] bytes = buffer.array(); // FIXME buffer must has array
        int offset = buffer.arrayOffset() + buffer.position();
        int length = buffer.arrayOffset() + buffer.limit();
        ByteArrayInputStream stream = new ByteArrayInputStream(bytes, offset,
                length);
        int startAvailable = stream.available();
        try{
            Unpacker unpacker = messagePack.createUnpacker(stream);// new MessagePackBufferUnpacker(messagePack,length);
            Value v = unpacker.readValue();
            source.skipBytes(startAvailable - stream.available());
            return v;
        }catch( EOFException e ){
            // not enough buffers.
            // So retry reading
            source.resetReaderIndex();
            return null;
        }

        /*
         * int noffset = pac.execute(bytes, offset, length); 
         * if(noffset > offset) { source.skipBytes(noffset - offset); }
         * if(pac.isFinished()) {
         *     Value msg = pac.getData(); pac.reset(); return msg;
         * } else { return null; }
         */
    }
}
