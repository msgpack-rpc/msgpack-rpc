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

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtMethod;
import javassist.CtNewConstructor;
import javassist.CtNewMethod;
import javassist.LoaderClassPath;
import javassist.NotFoundException;

import org.msgpack.MessagePack;
import org.msgpack.type.Value;
import org.msgpack.MessageTypeException;
import org.msgpack.template.Template;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.reflect.ReflectionInvokerBuilder.ReflectionArgumentEntry;
import org.msgpack.template.TemplateRegistry;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JavassistInvokerBuilder extends InvokerBuilder {
    private static Logger LOG = LoggerFactory.getLogger(JavassistInvokerBuilder.class);


	protected abstract static class AbstractInvoker implements Invoker {
        protected Method method;
        protected int parameterLength;
        protected ReflectionArgumentEntry[] entries;
        protected int minimumArrayLength;
        boolean async;

        public AbstractInvoker(Method method, ReflectionArgumentEntry[] entries, boolean async) {
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
                    throw new MessageTypeException();
                }

                int i;
                for(i = 0; i < minimumArrayLength; i++) {
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
                        e.convert(params,  obj);
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
                        e.convert(params, obj);
                    }
                }

                // latter entries are all Optional + nil => keep default value

            } catch (MessageTypeException e) {
                LOG.error("Fail to invoke",e);
            } catch (Exception e) {
                LOG.error("Fail to invoke",e);
            }

            try {
                Object result = invoke0(target, params);
                if(!async) {
                    request.sendResult(result);
                }
            } catch (Throwable t) {
                // TODO exception
            }
        }

        protected abstract Object invoke0(Object target, Object[] params) throws Throwable;
	}

    protected ClassPool pool;

    protected static int seqId = 0;

    protected StringBuilder stringBuilder = null;

    protected MessagePack messagePack;

    public JavassistInvokerBuilder(MessagePack messagePack) {
        pool = ClassPool.getDefault();
        this.messagePack = messagePack;
    }

    void addClassLoader(ClassLoader cl) {
        pool.appendClassPath(new LoaderClassPath(cl));
    }

    public Invoker buildInvoker(Method m, ArgumentEntry[] entries, boolean async) {
		ReflectionArgumentEntry[] res = new ReflectionArgumentEntry[entries.length];
		for(int i=0; i < entries.length; i++) {
			ArgumentEntry e = entries[i];
			Class<?> type = e.getType();
			if(!e.isAvailable()) {
				res[i] = new ReflectionInvokerBuilder.NullArgumentEntry(e);
			} else if(type.equals(boolean.class)) {
				res[i] = new ReflectionInvokerBuilder.BooleanArgumentEntry(e);
			} else if(type.equals(byte.class)) {
				res[i] = new ReflectionInvokerBuilder.ByteArgumentEntry(e);
			} else if(type.equals(short.class)) {
				res[i] = new ReflectionInvokerBuilder.ShortArgumentEntry(e);
			} else if(type.equals(int.class)) {
				res[i] = new ReflectionInvokerBuilder.IntArgumentEntry(e);
			} else if(type.equals(long.class)) {
				res[i] = new ReflectionInvokerBuilder.LongArgumentEntry(e);
			} else if(type.equals(float.class)) {
				res[i] = new ReflectionInvokerBuilder.FloatArgumentEntry(e);
			} else if(type.equals(double.class)) {
				res[i] = new ReflectionInvokerBuilder.DoubleArgumentEntry(e);
			} else {
                Type t = e.getGenericType();
				Template tmpl = messagePack.lookup(t);
                if(tmpl == null){
                    messagePack.register((Class<?>)t);
                    tmpl = messagePack.lookup(t);
                }
				res[i] = new ReflectionInvokerBuilder.ObjectArgumentEntry(messagePack,e, tmpl);
			}
		}
		return buildInvoker(m, res, async);
	}

    private Invoker buildInvoker(Method m, ReflectionArgumentEntry[] res, boolean async) {
        try {
            CtClass invokerCtClass = buildInvokerCtClass(m, res);
            Class<Invoker> invokerClass = buildInvokerClass(invokerCtClass);
            return (Invoker) newInvokerInstance(invokerClass, m, res, async);
        } catch (Throwable t) {
            t.printStackTrace();
            // FIXME
            // buildInvoker method should throw several exception (non-runtime exception)
            throw new RuntimeException(new NotBuiltException(
                    "invoker: " + m.getName() + " in " + m.getDeclaringClass().getName(), t));
        }
    }

    private CtClass buildInvokerCtClass(Method m, ReflectionArgumentEntry[] res)
            throws CannotCompileException, NotFoundException {
        Class<?> c = m.getDeclaringClass();
        String invokerClassName = c.getName() + "_$$_" + m.getName() + "_$$_JavassistInvoker" + nextSeqId();
        CtClass invokerCtClass = pool.makeClass(invokerClassName);
        buildSuperclass(invokerCtClass);
        buildConstructor(invokerCtClass);
        buildInvoke0Method(invokerCtClass, m, res);
        return invokerCtClass;
    }

    private int nextSeqId() {
        return seqId++;
    }

    private void buildSuperclass(CtClass invokerCtClass)
            throws CannotCompileException, NotFoundException {
        invokerCtClass.setSuperclass(pool.get(AbstractInvoker.class.getName()));
    }

    private void buildConstructor(CtClass invokerCtClass) throws CannotCompileException, NotFoundException {
        CtConstructor cons = CtNewConstructor.make(
                new CtClass[] {
                        pool.get(Method.class.getName()),
                        pool.get(ReflectionArgumentEntry.class.getName() + "[]"),
                        CtClass.booleanType
                },
                new CtClass[0], invokerCtClass);
        invokerCtClass.addConstructor(cons);
    }

    private void buildInvoke0Method(CtClass invokerCtClass, Method m, ReflectionArgumentEntry[] res)
            throws CannotCompileException, NotFoundException {
        int mod = javassist.Modifier.PUBLIC;
        CtClass retType = pool.get(Object.class.getName());
        String mname = "invoke0";
        String mbody = buildInvoke0MethodBody(m, res);
        CtClass[] paramTypes = new CtClass[] {
                pool.get(Object.class.getName()),
                pool.get(Object[].class.getName())
        };
        CtClass[] exceptTypes = new CtClass[] { pool.get(Throwable.class.getName()) };
        try {
            CtMethod newCtMethod = CtNewMethod.make(mod, retType, mname, 
                    paramTypes, exceptTypes, mbody, invokerCtClass);
            invokerCtClass.addMethod(newCtMethod);
        } catch (CannotCompileException e) {
            LOG.error("mbody src: " + mbody, e);
            throw e;
        }
    }

    private String buildInvoke0MethodBody(Method m, ReflectionArgumentEntry[] res) {
        // build an arguments string
        resetStringBuilder();
        Class<?>[] paramTypes = m.getParameterTypes();
        for (int i = 0; i < paramTypes.length; ++i) {
            Class<?> type = paramTypes[i];
            boolean isPrimType = paramTypes[i].isPrimitive();
            if (isPrimType) { // ((Integer) $2[0]).intValue()
                buildString("((%s) $2[%d]).%s()",
                        new Object[] { toWrapperType(type).getName(), i, toPrimTypeMethod(type) });
            } else { // (Integer) $2[0]
                buildString("(%s) $2[%d]", new Object[] { type.getName(), i });
            }
            if (i + 1 != paramTypes.length) {
                buildString(", ");
            }
        }
        String args = getBuiltString();
        
        boolean isVoidType = m.getReturnType().equals(void.class);
        boolean isPrimType = m.getReturnType().isPrimitive();
        resetStringBuilder();
        buildString("{ ");
        buildString("java.lang.Object ret = null; ");
        if (!isVoidType) {
            buildString("ret = ");
            if (isPrimType) {
                buildString("new %s(", new Object[] { toWrapperType(m.getReturnType()).getName() });
            }
        }
        buildString("((%s) $1).%s(%s)", new Object[] { m.getDeclaringClass().getName(), m.getName(), args });
        if (!isVoidType && isPrimType) {
            buildString(")");
        }
        buildString("; ");
        buildString("return ret; ");
        buildString("}");
        return getBuiltString();
    }

    protected static String toPrimTypeMethod(Class<?> type) {
        if (type.equals(boolean.class)) {
            return "booleanValue";
        } else if (type.equals(byte.class)) {
            return "byteValue";
        } else if (type.equals(short.class)) {
            return "shortValue";
        } else if (type.equals(int.class)) {
            return "intValue";
        } else if (type.equals(long.class)) {
            return "longValue";
        } else if (type.equals(float.class)) {
            return "floatValue";
        } else if (type.equals(double.class)) {
            return "doubleValue";
        } else {
            throw new UnsupportedOperationException("type: " + type.getName());
        }
    }

    protected static Class<?> toWrapperType(Class<?> type) {
        if (type.equals(boolean.class)) {
            return Boolean.class;
        } else if (type.equals(byte.class)) {
            return Byte.class;
        } else if (type.equals(short.class)) {
            return Short.class;
        } else if (type.equals(int.class)) {
            return Integer.class;
        } else if (type.equals(long.class)) {
            return Long.class;
        } else if (type.equals(float.class)) {
            return Float.class;
        } else if (type.equals(double.class)) {
            return Double.class;
        } else {
            throw new UnsupportedOperationException("type: " + type.getName());
        }
    }

    @SuppressWarnings("unchecked")
    private Class<Invoker> buildInvokerClass(CtClass invokerCtClass)
            throws CannotCompileException {
        return invokerCtClass.toClass(null, null);
    }

    private Invoker newInvokerInstance(Class<Invoker> c, Method m, ReflectionArgumentEntry[] res, boolean async)
            throws Exception {
        Constructor<Invoker> cons = c.getConstructor(
                new Class[] { Method.class, ReflectionArgumentEntry[].class, boolean.class }
        );
        return cons.newInstance(new Object[] { m, res, async });
    }

    protected void resetStringBuilder() {
        stringBuilder = new StringBuilder();
    }

    protected void buildString(String str) {
        stringBuilder.append(str);
    }

    protected void buildString(String format, Object... args) {
        stringBuilder.append(String.format(format, args));
    }

    protected String getBuiltString() {
        if(stringBuilder == null) {
            return null;
        }
        return stringBuilder.toString();
    }
}
