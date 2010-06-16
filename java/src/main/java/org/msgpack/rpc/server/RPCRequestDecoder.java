package org.msgpack.rpc.server;

import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.List;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.handler.codec.frame.FrameDecoder;
import org.msgpack.Unpacker;

public class RPCRequestDecoder extends FrameDecoder {
    protected final boolean isStream;
    protected Unpacker unpacker;

    public RPCRequestDecoder(boolean isStream) {
        super();
        this.isStream = isStream;
        this.unpacker = new Unpacker();
    }

    @Override
    protected Object decode(ChannelHandlerContext ctx, Channel channel, ChannelBuffer buffer) throws Exception {
        int len = buffer.readableBytes();
        if (len == 0) return null;
        
        // for message-based transport, reset unpacker for each message.
        if (!this.isStream)
            unpacker.reset();
        
        unpacker.reserveBuffer(len);
        byte[] unpacker_buf = unpacker.getBuffer();
        buffer.readBytes(unpacker_buf, unpacker.getBufferOffset(), len);
        unpacker.bufferConsumed(len);

        List<Object> ret = new ArrayList<Object>();
        while (unpacker.execute()) {
            Object data = unpacker.getData();
            unpacker.reset();
            if (data instanceof AbstractList<?>) {
                ret.add(data);
            } else {
                throw new IOException("invalid MPRPC" + data); // TODO
            }
        }
        return ret;
    }
}
