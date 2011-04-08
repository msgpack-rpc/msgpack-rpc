package org.msgpack.rpc;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import org.junit.Assert;
import org.junit.Test;
import org.msgpack.MessagePack;
import org.msgpack.MessagePackObject;
import org.msgpack.Packer;
import org.msgpack.annotation.MessagePackMessage;

import static org.hamcrest.CoreMatchers.*;


public class MethodTest  {

	@MessagePackMessage
	public static class ErrorSample{
		public ErrorSample(){}
		public ErrorSample(int code){this.code = code;}
		
		public int code;
		
		@Override
		public boolean equals(Object obj) {
			return obj instanceof ErrorSample && ((ErrorSample)obj).code == this.code;
		}
		@Override
		public String toString() {
			return "ErrorSample:" + code;
		}
	}
	@MessagePackMessage
	public static class ResultSample{
		public ResultSample(){}
		public ResultSample(String msg){resultMessage = msg;}
		public String resultMessage = "";

		@Override
		public boolean equals(Object obj) {
			return obj instanceof ResultSample && ((ResultSample)obj).resultMessage.endsWith(this.resultMessage);
		}
	}
	@Test
	public void testUnpackMethod() throws IOException{
		
		MessagePack.register(ErrorSample.class);
		MessagePack.register(ResultSample.class);
		
		int messageId = 1;
	    /*Object[] call = new Object[]{messageId , methodName , null , null};
	    MessagePack.pack(call);*/
		

		{
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			Packer packer = new Packer(out);
			packer.packArray(4);
			packer.packInt(1);
			packer.packInt(messageId);
			packer.pack(new ErrorSample(5));
			packer.packNil();
			
			
			MessagePackObject obj = MessagePack.unpack(out.toByteArray());
			
			MessagePackObject[] array = obj.asArray();
			Assert.assertThat(array.length, is(4));
			Assert.assertThat(array[0].asInt(), is(1));
			Assert.assertThat(array[1].asInt(), is(messageId));
			Assert.assertThat(array[2].convert(ErrorSample.class),is(new ErrorSample(5)));
			Assert.assertThat(array[3].convert(ResultSample.class), nullValue());
		}
		{
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			Packer packer = new Packer(out);
			packer.packArray(4);
			packer.packInt(1);
			packer.packInt(messageId);
			packer.packNil();
			packer.pack(new ResultSample("fuga"));
			
			
			MessagePackObject obj = MessagePack.unpack(out.toByteArray());
			
			MessagePackObject[] array = obj.asArray();
			Assert.assertThat(array.length, is(4));
			Assert.assertThat(array[0].asInt(), is(1));
			Assert.assertThat(array[1].asInt(), is(messageId));
			Assert.assertThat(array[2].convert(ErrorSample.class),nullValue());
			Assert.assertThat(array[3].convert(ResultSample.class),is(new ResultSample("fuga")));
		}
		
		{
			ByteArrayOutputStream out = new ByteArrayOutputStream();
			Packer packer = new Packer(out);
			packer.packArray(4);
			packer.packInt(1);
			packer.packInt(messageId);
			packer.packNil();
			packer.packNil();
			
			
			MessagePackObject obj = MessagePack.unpack(out.toByteArray());
			
			MessagePackObject[] array = obj.asArray();
			Assert.assertThat(array.length, is(4));
			Assert.assertThat(array[0].asInt(), is(1));
			Assert.assertThat(array[1].asInt(), is(messageId));
			Assert.assertThat(array[2].convert(ErrorSample.class),nullValue());
			Assert.assertThat(array[3].convert(ResultSample.class), nullValue());
		}
		
		
	}
}
