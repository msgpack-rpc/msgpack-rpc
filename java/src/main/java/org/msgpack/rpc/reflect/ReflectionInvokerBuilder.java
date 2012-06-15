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
package org.msgpack.rpc.reflect;

import java.io.IOException;
import java.lang.reflect.*;
import org.msgpack.*;
import org.msgpack.template.*;
import org.msgpack.rpc.*;
import org.msgpack.type.Value;
import org.msgpack.unpacker.Converter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ReflectionInvokerBuilder extends InvokerBuilder {

    private static final Logger logger = LoggerFactory.getLogger(ReflectionInvokerBuilder.class);
    protected  MessagePack messagePack;
    public ReflectionInvokerBuilder(MessagePack messagePack){
        this.messagePack = messagePack;
    }


	static abstract class ReflectionArgumentEntry extends ArgumentEntry {
		ReflectionArgumentEntry(ArgumentEntry e) {
			super(e);
		}

		public abstract void convert(Object[] params, Value obj) throws MessageTypeException;

		public void setNull(Object[] params) {
			params[getIndex()] = null;
		}
	}

	static class NullArgumentEntry extends ReflectionArgumentEntry {
		NullArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException { }
	}

	static class BooleanArgumentEntry extends ReflectionArgumentEntry {
		BooleanArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asBooleanValue().getBoolean();
		}
	}

	static class ByteArgumentEntry extends ReflectionArgumentEntry {
		ByteArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asIntegerValue().getByte();
		}
	}

	static class ShortArgumentEntry extends ReflectionArgumentEntry {
		ShortArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asIntegerValue().getShort();
		}
	}

	static class IntArgumentEntry extends ReflectionArgumentEntry {
		IntArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asIntegerValue().getInt();
		}
	}

	static class LongArgumentEntry extends ReflectionArgumentEntry {
		LongArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asIntegerValue().getLong();
		}
	}

	static class FloatArgumentEntry extends ReflectionArgumentEntry {
		FloatArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asFloatValue().getFloat();
		}
	}

	static class DoubleArgumentEntry extends ReflectionArgumentEntry {
		DoubleArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, Value obj) throws MessageTypeException {
			params[getIndex()] = obj.asFloatValue().getDouble();
		}
	}

	static class ObjectArgumentEntry extends ReflectionArgumentEntry {
		private Template template;

        private MessagePack messagePack;

		ObjectArgumentEntry(MessagePack messagePack,ArgumentEntry e, Template template) {
			super(e);
			this.template = template;
            this.messagePack = messagePack;
		}

		public void convert(Object[] params, Value obj) throws MessageTypeException {
            try {
                params[getIndex()] = template.read(new Converter(messagePack,obj),null);//messagePack.convert(obj,template);
            } catch (IOException e) {
                new MessageTypeException(e);
            }
        }
	}

	private static class ReflectionInvoker implements Invoker {
		protected Method method;
		protected int parameterLength;
		protected ReflectionArgumentEntry[] entries;
		protected int minimumArrayLength;
		boolean async;

		public ReflectionInvoker(Method method, ReflectionArgumentEntry[] entries, boolean async) {
			this.method = method;
			this.parameterLength = method.getParameterTypes().length;
			this.entries = entries;
			this.async = async;
			this.minimumArrayLength = 0;
			for(int i=0; i < entries.length; i++) {
				ReflectionArgumentEntry e = entries[i];
				if(!e.isOptional()){//e.isRequired() || e.isNullable()) {
					this.minimumArrayLength = i+1;
				}
			}
		}

		public void invoke(Object target, Request request) throws Exception {
			Object[] params = new Object[parameterLength];
			if(async) {
				params[0] = request;
			}

			// TODO set default values here

			try {
				Value args = request.getArguments();

				Value[] array = args.asArrayValue().getElementArray();
				int length = array.length;
				if(length < minimumArrayLength) {
					throw new MessageTypeException(String.format("Method needs at least %s args.But only %s args are passed",minimumArrayLength,length));
				}

				int i;
				for(i=0; i < minimumArrayLength; i++) {
					ReflectionArgumentEntry e = entries[i];
					if(!e.isAvailable()) {
						continue;
					}

					Value obj = array[i];
					if(obj.isNilValue()) {
						if(e.isRequired()) {
							// Required + nil => exception
							throw new MessageTypeException();
						} else if(e.isOptional()) {
							// Optional + nil => keep default value
						} else {  // Nullable
							// Nullable + nil => set null
							e.setNull(params);
						}
					} else {
                        try{
						    e.convert(params,  obj);
                        }catch(MessageTypeException mte){
                            logger.error(String.format("Expect Method:%s ArgIndex:%s Type:%s. But passed:%s",request.getMethodName(),i,e.getGenericType(),obj));
                            throw new MessageTypeException(String.format(
                                    "%sth argument type is %s.But wrong type is sent.",i+1,e.getJavaTypeName())
                                    );
                        }
					}
				}

				int max = length < entries.length ? length : entries.length;
				for(; i < max; i++) {
					ReflectionArgumentEntry e = entries[i];
					if(!e.isAvailable()) {
						continue;
					}

					Value obj = array[i];
					if(obj.isNilValue()) {
						// this is Optional field becaue i >= minimumArrayLength
						// Optional + nil => keep default value
					} else {

                        try{
						    e.convert(params, obj);
                        }catch(MessageTypeException mte){
                            logger.error(String.format("Expect Method:%s ArgIndex:%s Type:%s. But passed:%s",request.getMethodName(),i,e.getGenericType(),obj));
                            throw new MessageTypeException(String.format(
                                    "%sth argument type is %s.But wrong type is sent.",i+1,e.getJavaTypeName())
                            );
                        }
					}
				}

				// latter entries are all Optional + nil => keep default value

			} catch (MessageTypeException e) {
                throw e;
			} catch (Exception e) {
                //e.printStackTrace();
                throw e;
			}

			Object result = null;
            try{
                result = method.invoke(target, params);
            }catch(InvocationTargetException e ){
                if(e.getCause() != null && e.getCause() instanceof Exception){
                    throw (Exception)e.getCause();
                }else{
                    throw e;
                }
            }
			if(!async) {
				request.sendResult(result);
			}

			// TODO exception
		}
	}

	public Invoker buildInvoker(Method targetMethod, ArgumentEntry[] entries, boolean async) {
		int mod = targetMethod.getModifiers();
		if(!Modifier.isPublic(mod)) {
			targetMethod.setAccessible(true);
		}

		ReflectionArgumentEntry[] res = new ReflectionArgumentEntry[entries.length];
		for(int i=0; i < entries.length; i++) {
			ArgumentEntry e = entries[i];
			Class<?> type = e.getType();
			if(!e.isAvailable()) {
				res[i] = new NullArgumentEntry(e);
			} else if(type.equals(boolean.class)) {
				res[i] = new BooleanArgumentEntry(e);
			} else if(type.equals(byte.class)) {
				res[i] = new ByteArgumentEntry(e);
			} else if(type.equals(short.class)) {
				res[i] = new ShortArgumentEntry(e);
			} else if(type.equals(int.class)) {
				res[i] = new IntArgumentEntry(e);
			} else if(type.equals(long.class)) {
				res[i] = new LongArgumentEntry(e);
			} else if(type.equals(float.class)) {
				res[i] = new FloatArgumentEntry(e);
			} else if(type.equals(double.class)) {
				res[i] = new DoubleArgumentEntry(e);
			} else {

                Type t = e.getGenericType();
				Template tmpl = messagePack.lookup(t);
                if(tmpl == null){
                    messagePack.register((Class<?>)t);
                    tmpl = messagePack.lookup(t);
                }
				res[i] = new ObjectArgumentEntry(messagePack,e, tmpl);
			}
		}

		return new ReflectionInvoker(targetMethod, res, async);
	}
}

