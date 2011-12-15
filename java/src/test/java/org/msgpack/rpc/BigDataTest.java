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
package org.msgpack.rpc;

import org.msgpack.*;
import org.msgpack.rpc.*;
import org.msgpack.rpc.dispatcher.*;
import org.msgpack.rpc.config.*;
import org.msgpack.rpc.loop.*;
import org.msgpack.rpc.loop.netty.*;
import java.util.*;
import junit.framework.*;
import org.junit.Test;
import org.msgpack.template.Template;
import org.msgpack.type.Value;
import org.msgpack.type.ValueFactory;

public class BigDataTest extends TestCase {
	
    private static String getBigString() {
        StringBuilder sb = new StringBuilder(1024 * 1024); // 1M
        Random random = new Random();
        for(int i = 0;i < 1024 * 1024;i++){
            sb.append( (char)('a' + random.nextInt(26)));
        }
        return sb.toString();
    }

    private static Value BIG_DATA = ValueFactory.createRawValue(getBigString());
    public static class BigDataDispatcher implements Dispatcher {
		public void dispatch(Request request) {
            System.out.println(request.getArguments().asArrayValue().size());
            assertEquals(BIG_DATA,request.getArguments().asArrayValue().get(0) );
			request.sendResult(BIG_DATA);
		}
	}



	@Test
	public void testSyncBigDataLoad() throws Exception {
        MessagePack messagePack = new MessagePack();
		EventLoop loop = EventLoop.start(messagePack);
		Server svr = new Server(loop);
		Client c = new Client("127.0.0.1", 19851, loop);
		c.setRequestTimeout(10);


		try {
			svr.serve(new BigDataDispatcher());
			svr.listen(19851);

			int num = 5;

			long start = System.currentTimeMillis();
			for(int i=0; i < num; i++) {
				Value result = c.callApply("test", new Object[]{BIG_DATA});
				assertEquals(BIG_DATA, result);
			}
			long finish = System.currentTimeMillis();

			double result = num / ((double)(finish - start) / 1000);
			System.out.println("sync: "+result+" calls per sec");

		} finally {
			svr.close();
			c.close();
			loop.shutdown();
		}
    }
	@Test
	public void testAsyncBigDataLoad() throws Exception {
		EventLoop loop = EventLoop.start();
		Server svr = new Server(loop);
		Client c = new Client("127.0.0.1", 19852, loop);
		c.setRequestTimeout(100);//

		try {
			svr.serve(new BigDataDispatcher());
			svr.listen(19852);

			int num = 10;

			long start = System.currentTimeMillis();
			for(int i=0; i < num-1; i++) {
				c.notifyApply("test", new Object[]{BIG_DATA});
			}
			c.callApply("test", new Object[]{BIG_DATA});
			long finish = System.currentTimeMillis();

			double result = num / ((double)(finish - start) / 1000);
			System.out.println("async: "+result+" calls per sec");

		} finally {
			svr.close();
			c.close();
			loop.shutdown();
		}
	}
}

