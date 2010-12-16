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

import java.lang.reflect.*;
import org.msgpack.*;
import org.msgpack.template.*;
import org.msgpack.rpc.*;

public class ReflectionInvokerBuilder extends InvokerBuilder {
	private static ReflectionInvokerBuilder instance;
	public synchronized static ReflectionInvokerBuilder getInstance() {
		if(instance == null) {
			instance = new ReflectionInvokerBuilder();
		}
		return instance;
	}

	static abstract class ReflectionArgumentEntry extends ArgumentEntry {
		ReflectionArgumentEntry(ArgumentEntry e) {
			super(e);
		}

		public abstract void convert(Object[] params, MessagePackObject obj) throws MessageTypeException;

		public void setNull(Object[] params) {
			params[getIndex()] = null;
		}
	}

	static class NullArgumentEntry extends ReflectionArgumentEntry {
		NullArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException { }
	}

	static class BooleanArgumentEntry extends ReflectionArgumentEntry {
		BooleanArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asBoolean();
		}
	}

	static class ByteArgumentEntry extends ReflectionArgumentEntry {
		ByteArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asByte();
		}
	}

	static class ShortArgumentEntry extends ReflectionArgumentEntry {
		ShortArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asShort();
		}
	}

	static class IntArgumentEntry extends ReflectionArgumentEntry {
		IntArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asInt();
		}
	}

	static class LongArgumentEntry extends ReflectionArgumentEntry {
		LongArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asLong();
		}
	}

	static class FloatArgumentEntry extends ReflectionArgumentEntry {
		FloatArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asFloat();
		}
	}

	static class DoubleArgumentEntry extends ReflectionArgumentEntry {
		DoubleArgumentEntry(ArgumentEntry e) {
			super(e);
		}
		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.asDouble();
		}
	}

	static class ObjectArgumentEntry extends ReflectionArgumentEntry {
		private Template template;

		ObjectArgumentEntry(ArgumentEntry e, Template template) {
			super(e);
			this.template = template;
		}

		public void convert(Object[] params, MessagePackObject obj) throws MessageTypeException {
			params[getIndex()] = obj.convert(template);
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
				if(e.isRequired() || e.isNullable()) {
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
				MessagePackObject args = request.getArguments();

				MessagePackObject[] array = args.asArray();
				int length = array.length;
				if(length < minimumArrayLength) {
					throw new MessageTypeException();
				}

				int i;
				for(i=0; i < minimumArrayLength; i++) {
					ReflectionArgumentEntry e = entries[i];
					if(!e.isAvailable()) {
						continue;
					}

					MessagePackObject obj = array[i];
					if(obj.isNil()) {
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
						e.convert(params,  obj);
					}
				}

				int max = length < entries.length ? length : entries.length;
				for(; i < max; i++) {
					ReflectionArgumentEntry e = entries[i];
					if(!e.isAvailable()) {
						continue;
					}

					MessagePackObject obj = array[i];
					if(obj.isNil()) {
						// this is Optional field becaue i >= minimumArrayLength
						// Optional + nil => keep default value
					} else {
						e.convert(params, obj);
					}
				}

				// latter entries are all Optional + nil => keep default value

			} catch (MessageTypeException e) {
			} catch (Exception e) {
			}

			Object result = method.invoke(target, params);
			if(!async) {
				request.sendResult(result);
			}

			// TODO exception
		}
	}

	public Invoker buildInvoker(Object target, Method targetMethod, ArgumentEntry[] entries, boolean async) {
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
				Template tmpl = TemplateRegistry.lookup(e.getGenericType(), true);
				res[i] = new ObjectArgumentEntry(e, tmpl);
			}
		}

		return new ReflectionInvoker(targetMethod, res, async);
	}
}

