package org.msgpack.rpc.client.netty;

import static org.jboss.netty.channel.Channels.*;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.msgpack.rpc.client.TCPSocket;

public class RPCClientPipelineFactory implements ChannelPipelineFactory {
    protected TCPSocket sock;
    
    public RPCClientPipelineFactory(TCPSocket sock) {
        this.sock = sock;
    }

    public ChannelPipeline getPipeline() throws Exception {
        ChannelPipeline pipeline = pipeline();
        pipeline.addLast("encoder", new RPCRequestEncoder());        
        pipeline.addLast("decoder", new RPCResponseDecoder());
        pipeline.addLast("client", new RPCClientHandler(sock));
        return pipeline;
    }
}
