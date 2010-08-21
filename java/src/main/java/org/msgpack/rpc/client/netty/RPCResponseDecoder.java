//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 Kazuki Ohta
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
package org.msgpack.rpc.client.netty;

import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.handler.codec.frame.FrameDecoder;
import org.msgpack.Unpacker;
import org.msgpack.rpc.client.RPCException;

public class RPCResponseDecoder extends FrameDecoder {
    protected Unpacker unpacker;

    public RPCResponseDecoder() {
        super();
        this.unpacker = new Unpacker();
    }

    @Override
    protected Object decode(ChannelHandlerContext ctx, Channel channel, ChannelBuffer buffer) throws Exception {
        int len = buffer.readableBytes();
        if (len == 0) return null;
        
        unpacker.reserveBuffer(len);
        byte[] unpacker_buf = unpacker.getBuffer();
        buffer.readBytes(unpacker_buf, unpacker.getBufferOffset(), len);
        unpacker.bufferConsumed(len);

        // 2010/06/16 Kazuki Ohta <kazuki.ohta@gmail.com>
        // This function is called when netty receives some data. If multiple
        // messages are sent through one connection, the buffer contains all of
        // them. Therefore, we need to continue unpack until unpacker.execute()
        // returns the false.
        List<Object> ret = new ArrayList<Object>();
        while (unpacker.execute()) {
            Object data = unpacker.getData();
            unpacker.reset();
            if (data instanceof AbstractList<?>) {
                ret.add(data);
            } else {
                throw new RPCException("invalid MPRPC" + data);
            }
        }
        return ret;
    }
}
