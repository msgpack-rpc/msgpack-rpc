package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtNewConstructor;
import javassist.CtNewMethod;
import javassist.NotFoundException;

import org.msgpack.MessagePackObject;
import org.msgpack.rpc.Dispatcher;
import org.msgpack.rpc.Request;

public class DynamicCodegenDispatcher implements Dispatcher {

    static class Constants {
        static final String POSTFIX_TYPE_NAME_INVOKER = "_$$_Invoker";

        static final String KEYWORD_MODIFIER_PUBLIC = "public";

        static final String KEYWORD_THROWS = "throws";

        static final String KEYWORD_CATCH = "catch";

        static final String KEYWORD_NEW = "new";

        static final String KEYWORD_TRY = "try";

        static final String KEYWORD_NULL = "null";

        static final String CHAR_NAME_SPACE = " ";

        static final String CHAR_NAME_DOT = ".";

        static final String CHAR_NAME_COMMA = ",";

        static final String CHAR_NAME_EQUAL = "=";

        static final String CHAR_NAME_UNDERSCORE = "_";

        static final String CHAR_NAME_SEMICOLON = ";";

        static final String CHAR_NAME_RIGHT_PARENTHESIS = ")";

        static final String CHAR_NAME_LEFT_PARENTHESIS = "(";

        static final String CHAR_NAME_RIGHT_CURLY_BRACKET = "}";

        static final String CHAR_NAME_LEFT_CURLY_BRACKET = "{";

        static final String CHAR_NAME_RIGHT_SQUARE_BRACKET = "]";

        static final String CHAR_NAME_LEFT_SQUARE_BRACKET = "[";

        static final String METHOD_NAME_INVOKE = "invoke";

        static final String METHOD_NAME_GETARGUMENTS = "getArguments";

        static final String METHOD_NAME_SENDRESPONSE = "sendResponse";

        static final String FIELD_NAME_TARGET = "_$$_target";

        static final String VARIABLE_NAME_TARGET = "_$$_t";

        static final String VARIABLE_NAME_ARGS = "_$$_";

        static final String VARIABLE_NAME_REQUEST = "_$$_r";

        static final String VARIABLE_NAME_MESSAGEPACKOBJECTS = "_$$_mpos";

        static final String VARIABLE_NAME_ERROR = "_$$_err";

        static final String VARIABLE_NAME_RET = "_$$_ret";
    }

    public static class InvokersGenerator {
        private ClassPool pool;

        private ConcurrentHashMap<String, Map<String, Class<?>>> classesCache;

        public InvokersGenerator() {
            pool = ClassPool.getDefault();
            classesCache = new ConcurrentHashMap<String, Map<String, Class<?>>>();
        }

        public Map<String, Class<?>> getCache(String origName) {
            return classesCache.get(origName);
        }

        public void setCache(String origName, Map<String, Class<?>> cache) {
            classesCache.put(origName, cache);
        }

        public Map<String, Class<?>> generateInvokerClasses(Object origObj,
                Class<?> origClass) throws NotFoundException,
                CannotCompileException {
            String origName = origClass.getName();
            Map<String, Class<?>> cache = classesCache.get(origName);
            if (cache != null) {
                return cache;
            }
            Map<String, Class<?>> invokerClasses = generateInvokerClasses(
                    origName, origClass);
            classesCache.put(origName, invokerClasses);
            return invokerClasses;
        }

        private Map<String, Class<?>> generateInvokerClasses(String origName,
                Class<?> origClass) throws NotFoundException,
                CannotCompileException {
            checkClassValidation(origClass);
            Method[] methods = getDeclaredMethods(origClass);
            Map<String, Class<?>> invokerClasses = new HashMap<String, Class<?>>();
            for (Method method : methods) {
                try {
                    Class<?> invokerClass = generateInvokerClass(origClass,
                            method);
                    invokerClasses.put(method.getName(), invokerClass);
                } catch (Exception e) { // ignore
                    //e.printStackTrace();
                }
            }
            return invokerClasses;
        }

        private void checkClassValidation(Class<?> origClass) {
            // not public, abstract
            int mod = origClass.getModifiers();
            if ((!Modifier.isPublic(mod)) || Modifier.isAbstract(mod)) {
                throwClassValidationException(origClass,
                        "it must be a public class");
            }
        }

        private static void throwClassValidationException(Class<?> origClass,
                String message) {
            throw new DynamicCodegenException(message + ": "
                    + origClass.getName());
        }

        private Method[] getDeclaredMethods(Class<?> origClass) {
            ArrayList<Method> allMethods = new ArrayList<Method>();
            Class<?> nextClass = origClass;
            while (!nextClass.equals(Object.class)) {
                Method[] methods = nextClass.getDeclaredMethods();
                for (Method method : methods) {
                    try {
                        checkMethodValidation(method, allMethods);
                        allMethods.add(method);
                    } catch (Exception e) { // ignore
                    }
                }
                nextClass = nextClass.getSuperclass();
            }
            return allMethods.toArray(new Method[0]);
        }

        private void checkMethodValidation(Method method, List<Method> methods) {
            // check modifiers (public)
            int mod = method.getModifiers();
            if ((!Modifier.isPublic(mod)) || Modifier.isStatic(mod)
                    || method.isBridge() || method.isSynthetic()
                    || method.isVarArgs()) {
                throwMethodValidationException(method,
                        "it must be a non-static public method");
            }

            // check same name
            for (Method m : methods) {
                if (m.getName().equals(method.getName())) {
                    throwMethodValidationException(method,
                            "a same name of the method");
                }
            }

            // check a method that has a 1st parameter of specific type
            Class<?>[] types = method.getParameterTypes();
            if (types.length != 0) {
                if (types[0].equals(Request.class)) {
                    for (int i = 1; i < types.length; ++i) {
                        if (types[i].equals(Request.class)) {
                            throwMethodValidationException(method,
                                    "type error: param #" + i);
                        }
                    }
                    Class<?> retType = method.getReturnType();
                    if (!retType.equals(void.class)) {
                        throwMethodValidationException(method,
                                "its return type must be void");
                    }
                }
            }
        }

        private static void throwMethodValidationException(Method method,
                String message) {
            throw new DynamicCodegenException(message + ": " + method.getName());
        }

        private Class<?> generateInvokerClass(Class<?> origClass, Method method)
                throws NotFoundException, CannotCompileException, Exception {
            // TODO must refine the logic for enhanced class 
            String origName = origClass.getName();
            CtClass invokerCtClass = makeClass(origName, method);
            setInterface(invokerCtClass);
            addField(invokerCtClass, origClass);
            addConstructor(invokerCtClass, origClass);
            addInvokeMethod(invokerCtClass, method);
            return createClass(invokerCtClass);
        }

        private CtClass makeClass(String origName, Method method)
                throws NotFoundException {
            StringBuilder sb = new StringBuilder();
            sb.append(origName);
            sb.append(Constants.CHAR_NAME_UNDERSCORE);
            sb.append(method.getName());
            sb.append(Constants.POSTFIX_TYPE_NAME_INVOKER);
            String invokerName = sb.toString();
            CtClass invokerCtClass = pool.makeClass(invokerName);
            invokerCtClass.setModifiers(Modifier.PUBLIC);
            return invokerCtClass;
        }

        private void setInterface(CtClass invokerCtClass)
                throws NotFoundException {
            CtClass invokerInf = pool.get(Invoker.class.getName());
            invokerCtClass.addInterface(invokerInf);
        }

        private void addField(CtClass invokerCtClass, Class<?> origClass)
                throws CannotCompileException {
            // in this part, a created field is not initialized
            StringBuilder sb = new StringBuilder();
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(origClass.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.FIELD_NAME_TARGET);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            //System.out.println("invoker field: " + sb.toString());
            CtField targetCtField = CtField.make(sb.toString(), invokerCtClass);
            invokerCtClass.addField(targetCtField);
        }

        private void addConstructor(CtClass invokerCtClass, Class<?> origClass)
                throws CannotCompileException {
            StringBuilder sb = new StringBuilder();
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(invokerCtClass.getSimpleName());
            sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            sb.append(origClass.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_TARGET);
            sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_LEFT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.FIELD_NAME_TARGET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_TARGET);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_RIGHT_CURLY_BRACKET);
            //System.out.println("invoker constructor: " + sb.toString());
            CtConstructor newCtConstructor = CtNewConstructor.make(sb
                    .toString(), invokerCtClass);
            invokerCtClass.addConstructor(newCtConstructor);
        }

        private void addInvokeMethod(CtClass invokerCtClass, Method method)
                throws CannotCompileException {
            StringBuilder sb = new StringBuilder();
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC);
            sb.append(Constants.CHAR_NAME_SPACE);
            // return type
            sb.append(void.class.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            // method name
            sb.append(Constants.METHOD_NAME_INVOKE);
            // params
            sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            sb.append(Request.class.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_REQUEST);
            sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_SPACE);
            insertInvokeMethodBody(sb, method);
            //System.out.println("invoker method: " + sb.toString());
            CtMethod newCtMethod = CtNewMethod.make(sb.toString(),
                    invokerCtClass);
            invokerCtClass.addMethod(newCtMethod);
        }

        private void insertInvokeMethodBody(StringBuilder sb, Method method) {
            sb.append(Constants.CHAR_NAME_LEFT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            // MessagePackObject[] packObjs = req.getArguments();
            insertGetArguments(sb);
            // int i = packObjs[0].intValue();
            insertTypeConversionOfLocalVariables(sb, method);
            // Throwable err = null; Object ret = null;
            insertLocalErrorAndResultVariables(sb);
            // try { } catch (Throwable t0) { t = t0; }
            insertTryCatch(sb, method);
            // req.sendResponse(ret, err);
            insertSendResponseCall(sb);
            sb.append(Constants.CHAR_NAME_RIGHT_CURLY_BRACKET);
        }

        private void insertGetArguments(StringBuilder sb) {
            // MessagePackObject[] packObjs = req.getArguments();
            sb.append(MessagePackObject.class.getName());
            sb.append(Constants.CHAR_NAME_LEFT_SQUARE_BRACKET);
            sb.append(Constants.CHAR_NAME_RIGHT_SQUARE_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_MESSAGEPACKOBJECTS);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_REQUEST);
            sb.append(Constants.CHAR_NAME_DOT);
            sb.append(Constants.METHOD_NAME_GETARGUMENTS);
            sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
        }

        private void insertTypeConversionOfLocalVariables(StringBuilder sb,
                Method method) {
            Class<?>[] types = method.getParameterTypes();
            int j = 0;
            for (int i = 0; i < types.length; ++i) {
                if (!types[i].equals(Request.class)) {
                    insertTypeConversionOfLocalVariable(sb, j, types[i]);
                    ++j;
                }
            }
        }

        private void insertTypeConversionOfLocalVariable(StringBuilder sb,
                int index, Class<?> type) {
            // int _$$_0 = packObjs[0].intValue();
            sb.append(type.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ARGS);
            sb.append(index);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            StringBuilder varsb = new StringBuilder();
            varsb.append(Constants.VARIABLE_NAME_MESSAGEPACKOBJECTS);
            varsb.append(Constants.CHAR_NAME_LEFT_SQUARE_BRACKET);
            varsb.append(index);
            varsb.append(Constants.CHAR_NAME_RIGHT_SQUARE_BRACKET);
            insertJavaType(sb, varsb.toString(), type);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
        }

        private void insertJavaType(StringBuilder sb, String name, Class<?> type) {
            if (type.isPrimitive()) { // primitive type
                sb.append(name);
                sb.append(Constants.CHAR_NAME_DOT);
                if (type.equals(boolean.class)) { // boolean
                    sb.append("asBoolean");
                } else if (type.equals(byte.class)) { // byte
                    sb.append("byteValue");
                } else if (type.equals(short.class)) { // short
                    sb.append("shortValue");
                } else if (type.equals(int.class)) { // int
                    sb.append("intValue");
                } else if (type.equals(long.class)) { // long
                    sb.append("longValue");
                } else if (type.equals(float.class)) { // float
                    sb.append("floatValue");
                } else if (type.equals(double.class)) { // double
                    sb.append("doubleValue");
                } else if (type.equals(void.class)) { // void
                    throw new DynamicCodegenException("fatal error: "
                            + type.getName());
                } else {
                    throw new DynamicCodegenException("fatal error: "
                            + type.getName());
                }
                sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            } else { // reference type
                // wrapper type
                // new Boolean(name.asBoolean())
                if (type.equals(Boolean.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Boolean.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("asBoolean");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Byte.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Byte.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("byteValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Short.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Short.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("shortValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Integer.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Integer.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("intValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Long.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Long.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("longValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Float.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Float.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("floatValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(Double.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Double.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(name);
                    sb.append(Constants.CHAR_NAME_DOT);
                    sb.append("doubleValue");
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else {
                    if (type.equals(String.class)) {
                        sb.append(name);
                        sb.append(Constants.CHAR_NAME_DOT);
                        sb.append("asString");
                        sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                        sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    } else if (type.equals(BigInteger.class)) {
                        sb.append(name);
                        sb.append(Constants.CHAR_NAME_DOT);
                        sb.append("bigIntegerValue");
                        sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                        sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    } else if (type.equals(byte[].class)) {
                        sb.append(name);
                        sb.append(Constants.CHAR_NAME_DOT);
                        sb.append("asByteArray");
                        sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                        sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                    } else { // TODO
                        throw new UnsupportedOperationException("fatal error: "
                                + type.getName());
                    }
                }
            }
        }

        private void insertLocalErrorAndResultVariables(StringBuilder sb) {
            // Throwable err = null; Object ret = null;
            sb.append(Throwable.class.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ERROR);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.KEYWORD_NULL);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Object.class.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_RET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.KEYWORD_NULL);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
        }

        private void insertTryCatch(StringBuilder sb, Method method) {
            // try { } catch (Throwable t0) { t = t0; }
            sb.append(Constants.KEYWORD_TRY);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_LEFT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            // ret = new Integer(handler.m0(i, j));
            if (!method.getReturnType().equals(void.class)) {
                sb.append(Constants.VARIABLE_NAME_RET);
                sb.append(Constants.CHAR_NAME_SPACE);
                sb.append(Constants.CHAR_NAME_EQUAL);
                sb.append(Constants.CHAR_NAME_SPACE);
            }
            StringBuilder retsb = new StringBuilder();
            retsb.append(Constants.FIELD_NAME_TARGET);
            retsb.append(Constants.CHAR_NAME_DOT);
            retsb.append(method.getName());
            retsb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            Class<?>[] types = method.getParameterTypes();
            int j = 0;
            for (int i = 0; i < types.length; ++i) {
                if (types[i].equals(Request.class)) {
                    retsb.append(Constants.VARIABLE_NAME_REQUEST);
                } else {
                    retsb.append(Constants.VARIABLE_NAME_ARGS);
                    retsb.append(j);
                    ++j;
                }
                if (i + 1 != types.length) {
                    retsb.append(Constants.CHAR_NAME_COMMA);
                    retsb.append(Constants.CHAR_NAME_SPACE);
                }
            }
            retsb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            insertTypeConversionOfReturn(sb, retsb.toString(), method
                    .getReturnType());
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_RIGHT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.KEYWORD_CATCH);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            sb.append(Throwable.class.getName());
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ERROR + "0");
            sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_LEFT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ERROR);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_EQUAL);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ERROR + "0");
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.CHAR_NAME_RIGHT_CURLY_BRACKET);
            sb.append(Constants.CHAR_NAME_SPACE);
        }

        private void insertTypeConversionOfReturn(StringBuilder sb,
                String expr, Class<?> type) {
            // ret = new Integer(...); ret is the Object type
            if (type.isPrimitive()) { // primitive type
                if (type.equals(boolean.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Boolean.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(byte.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Byte.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(short.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Short.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(int.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Integer.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(long.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Long.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(float.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Float.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(double.class)) {
                    sb.append(Constants.KEYWORD_NEW);
                    sb.append(Constants.CHAR_NAME_SPACE);
                    sb.append(Double.class.getName());
                    sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
                    sb.append(expr);
                    sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
                } else if (type.equals(void.class)) {
                    sb.append(expr);
                } else {
                    throw new UnsupportedOperationException("fatal error: "
                            + type.getName());
                }
            } else { // reference type
                return;
            }
        }

        private void insertSendResponseCall(StringBuilder sb) {
            // req.sendResponse(ret, err);
            sb.append(Constants.VARIABLE_NAME_REQUEST);
            sb.append(Constants.CHAR_NAME_DOT);
            sb.append(Constants.METHOD_NAME_SENDRESPONSE);
            sb.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            sb.append(Constants.VARIABLE_NAME_RET);
            sb.append(Constants.CHAR_NAME_COMMA);
            sb.append(Constants.CHAR_NAME_SPACE);
            sb.append(Constants.VARIABLE_NAME_ERROR);
            sb.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            sb.append(Constants.CHAR_NAME_SEMICOLON);
            sb.append(Constants.CHAR_NAME_SPACE);
        }

        private Class<?> createClass(CtClass invokerCtClass)
                throws CannotCompileException {
            return invokerCtClass.toClass(null, null);
        }
    }

    public interface Invoker {
        void invoke(Request reqest);
    }

    private static InvokersGenerator gen;

    private Map<String, Invoker> invokersCache = new ConcurrentHashMap<String, Invoker>();

    private Invoker getCache(String methodName) {
        return invokersCache.get(methodName);
    }

    private void setCache(String methodName, Invoker invoker) {
        if (invoker != null) {
            invokersCache.put(methodName, invoker);
        }
    }

    public DynamicCodegenDispatcher(Object origObj) {
        if (gen == null) {
            gen = new InvokersGenerator();
        }

        Class<?> origClass = origObj.getClass();
        String origName = origClass.getName();
        Map<String, Class<?>> classCache = null;
        try {
            if ((classCache = gen.getCache(origName)) == null) {
                // generate invoker classes related to the original class
                try {
                    classCache = gen.generateInvokerClasses(origObj, origClass);
                } catch (NotFoundException e) {
                    throw new DynamicCodegenException(e.getMessage(), e);
                } catch (CannotCompileException e) {
                    throw new DynamicCodegenException(e.getMessage(), e);
                }
                // set the generated invoker classes to a cache
                gen.setCache(origName, classCache);
            }

            // create a new invoker object
            if (classCache != null) {
                newInvokerInstances(origObj, origClass, classCache);
            }
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    private void newInvokerInstances(Object origObj, Class<?> origClass,
            Map<String, Class<?>> invokerClasses) {
        Iterator<String> methodNames = invokerClasses.keySet().iterator();
        while (methodNames.hasNext()) {
            String methodName = methodNames.next();
            Invoker invoker = null;
            try {
                Class<?> invokerClass = invokerClasses.get(methodName);
                if (invokerClass == null) {
                    continue;
                }
                invoker = newInvokerInstance(origObj, origClass, invokerClass);
            } catch (Exception e) {
                continue;
            }

            if (invoker != null) {
                setCache(methodName, invoker);
            }
        }
    }

    private Invoker newInvokerInstance(Object target, Class<?> origClass,
            Class<?> invokerClass) throws Exception {
        Constructor<?> cons = invokerClass
                .getConstructor(new Class[] { origClass });
        return (Invoker) cons.newInstance(target);
    }

    @Override
    public void dispatch(Request request) throws Exception {
        Invoker invoker = getCache(request.getMethodName());
        if (invoker == null) {
            throw new DynamicCodegenException("Invoker not found: "
                    + request.getMethodName());
        }
        invoker.invoke(request);
    }
}