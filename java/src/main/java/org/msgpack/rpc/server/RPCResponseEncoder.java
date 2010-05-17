package org.msgpack.rpc.server;

import java.io.ByteArrayOutputStream;

import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder;
import org.msgpack.Packer;

@ChannelPipelineCoverage("all")
public class RPCResponseEncoder extends OneToOneEncoder {
    @Override
    protected Object encode(ChannelHandlerContext ctx, Channel channel,
            Object msg) throws Exception {
        ByteArrayOutputStream o = new ByteArrayOutputStream();
        new Packer(o).pack(msg);
        byte[] b = o.toByteArray();
        ChannelBuffer buf = ChannelBuffers.dynamicBuffer();
        buf.writeBytes(b);
        return buf;
    }
}
