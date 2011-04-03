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

import org.jboss.netty.channel.Channel;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.buffer.ChannelBuffer;
import org.jboss.netty.buffer.ChannelBufferOutputStream;
import org.jboss.netty.buffer.ChannelBuffers;
import org.jboss.netty.handler.codec.oneone.OneToOneEncoder;
import org.msgpack.MessagePack;

public class MessagePackEncoder extends OneToOneEncoder {
	private final int estimatedLength;

	public MessagePackEncoder() {
		this(1024);
	}

	public MessagePackEncoder(int estimatedLength) {
		this.estimatedLength = estimatedLength;
	}

	@Override
	protected Object encode(
			ChannelHandlerContext ctx, Channel channel,
			Object msg) throws Exception {
		if(msg instanceof ChannelBuffer) {
			return msg;
		}

		ChannelBufferOutputStream out =
			new ChannelBufferOutputStream(
					ChannelBuffers.dynamicBuffer(
						estimatedLength,
						ctx.getChannel().getConfig().getBufferFactory()));

		MessagePack.pack(out, msg);

		ChannelBuffer result = out.buffer();
		return result;
	}
}

