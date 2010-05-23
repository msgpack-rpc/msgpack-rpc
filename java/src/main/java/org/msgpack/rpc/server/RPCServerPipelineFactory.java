package org.msgpack.rpc.server;

import static org.jboss.netty.channel.Channels.*;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;

public class RPCServerPipelineFactory implements ChannelPipelineFactory {
    private final RPCServerHandler handler;
    private final RPCResponseEncoder encoder;
    private final boolean isStream;

    public RPCServerPipelineFactory(Object userHandler, boolean isStream) {
        this.handler = new RPCServerHandler(userHandler);
        this.encoder = new RPCResponseEncoder();
        this.isStream = isStream;
    }

    public ChannelPipeline getPipeline() throws Exception {
        //MemoryAwareThreadPoolExecutor eventExecutor =
        //    new MemoryAwareThreadPoolExecutor(5, 1000000, 10000000, 100,
        //                                      TimeUnit.MILLISECONDS);
        ChannelPipeline pipeline = pipeline();
        pipeline.addLast("decoder", new RPCRequestDecoder(isStream));
        pipeline.addLast("encoder", encoder);
        //pipeline.addLast("executor", new ExecutionHandler(eventExecutor));                                                                                                      
        pipeline.addLast("handler", handler);
        return pipeline;
    }
}
