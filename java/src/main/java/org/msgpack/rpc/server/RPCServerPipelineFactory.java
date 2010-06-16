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
