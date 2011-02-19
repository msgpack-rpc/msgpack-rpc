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
import org.jboss.netty.handler.codec.frame.FrameDecoder;
import org.msgpack.Unpacker;
import org.msgpack.MessagePackObject;

public class MessagePackStreamDecoder extends FrameDecoder {
	protected Unpacker pac = new Unpacker();

	public MessagePackStreamDecoder() {
		super();
	}

	@Override
	protected Object decode(
			ChannelHandlerContext ctx, Channel channel,
			ChannelBuffer source) throws Exception {
		ByteBuffer buffer = source.toByteBuffer();
		if(!buffer.hasRemaining()) {
			return null;
		}

		byte[] bytes = buffer.array();  // FIXME buffer must has array
		int offset = buffer.arrayOffset() + buffer.position();
		int length = buffer.arrayOffset() + buffer.limit();

		int noffset = pac.execute(bytes, offset, length);
		if(noffset > offset) {
			source.skipBytes(noffset - offset);
		}

		if(pac.isFinished()) {
			MessagePackObject msg = pac.getData();
			pac.reset();
			return msg;
		} else {
			return null;
		}
	}
}

