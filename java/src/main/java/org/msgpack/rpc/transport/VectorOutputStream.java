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
package org.msgpack.rpc.transport;

import java.io.*;
import java.util.*;
import java.nio.*;
import java.nio.channels.*;
import org.msgpack.*;

class VectorOutputStream extends OutputStream {
	private static class OpenByteArrayOutputStream extends ByteArrayOutputStream {
		public OpenByteArrayOutputStream() {
			super();
		}

		public byte[] getBuffer() {
			return buf;
		}
	}

	private OpenByteArrayOutputStream out = new OpenByteArrayOutputStream();
	int offset = 0;

	public VectorOutputStream() {
		super();
	}

	public boolean isEmpty() {
		return out.size() == 0;
	}

	public void write(byte[] b) {
		try {
			out.write(b);
		} catch (IOException e) { }
	}

	public void write(byte[] b, int off, int len) {
		out.write(b, off, len);
	}

	public void write(int b) {
		out.write(b);
	}

	public void reset() {
		out.reset();
		offset = 0;
	}

	public int read(GatheringByteChannel to) throws IOException {
		int maxSize = out.size() - offset;
		int count = to.write(ByteBuffer.wrap(out.getBuffer(), offset, maxSize));
		if(maxSize <= count) {
			out.reset();
			offset = 0;
		} else {
			count += maxSize;
		}
		return count;
	}

	// FIXME
	byte[] getBuffer() {
		return out.getBuffer();
	}
	int size() {
		return out.size();
	}


	/* TODO
	private List<ByteBuffer> buffers = new ArrayList<ByteBuffer>();
	private int lastFreeSize = 0;

	private int threshold = 1024;
	private int chunkSize = 8192;

	public void read(GatheringByteChannel to) {
		// FIXME
	}

	public void close() {
		// FIXME
	}

	public void flush() {
	}

	public void write(byte[] b) {
		if(b.length > threshold) {
			writeReference(b, 0, b.length);
		} else {
			writeCopy(b, 0, b.length);
		}
	}

	public void write(byte[] b, int off, int len) {
		if(len > threshold) {
			writeReference(b, off, len);
		} else {
			writeCopy(b, off, len);
		}
	}

	public void write(int b) {
	}

	public synchronized void writeReference(byte[] b, int off, int len) {
		buffers.add(ByteBuffer.wrap(b, off, len));
		lastFreeSize = 0;
	}

	public synchronized void writeCopy(byte[] b, int off, int len) {
		if(lastFreeSize > len) {
		} else {
			int buflen = chunkSize > len ? chunkSize : len;
		} else {
		}
	}
	*/
}

