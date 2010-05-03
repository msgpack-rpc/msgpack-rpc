package org.msgpack.rpc.client.netty;

import java.io.IOException;
import java.util.AbstractList;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.handler.codec.frame.FrameDecoder;
import org.msgpack.Unpacker;

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

        if (unpacker.execute()) {
            Object data = unpacker.getData();
            unpacker.reset();
            if (data instanceof AbstractList)
                return data;
            throw new IOException("invalid MPRPC" + data); // TODO                                                                                                                
        }
        return null;
    }
}
