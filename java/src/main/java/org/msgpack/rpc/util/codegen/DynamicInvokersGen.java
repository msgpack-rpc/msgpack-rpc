package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
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

import org.msgpack.CustomConverter;
import org.msgpack.MessageConvertable;
import org.msgpack.MessagePackObject;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.util.codegen.DynamicCodeGenDispatcher.Invoker;
import org.msgpack.util.codegen.DynamicCodeGenBase;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DynamicInvokersGen extends DynamicCodeGenBase implements Constants {
    private static Logger LOG = LoggerFactory.getLogger(DynamicInvokersGen.class);
    
    private ClassPool pool;

    private ConcurrentHashMap<String, Map<String, Class<?>>> classesCache;

    public DynamicInvokersGen() {
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
            Class<?> origClass) throws DynamicCodeGenException {
        String origName = origClass.getName();
        Map<String, Class<?>> cache = classesCache.get(origName);
        if (cache != null) {
            return cache;
        }
        Map<String, Class<?>> classes = null;
        try {
            classes = generateInvokerClasses(origName, origClass);
        } catch (DynamicCodeGenException e) {
            // TODO for debug
            e.printStackTrace();
            throw e;
        }
        if (classes != null) {
            classesCache.put(origName, classes);
        }
        return classes;
    }

    private Map<String, Class<?>> generateInvokerClasses(String origName,
            Class<?> origClass) throws DynamicCodeGenException {
        checkClassValidation(origClass);
        Method[] methods = getDeclaredMethods(origClass);
        Map<String, Class<?>> classes = new HashMap<String, Class<?>>();
        for (Method method : methods) {
            try {
                Class<?> invokerClass = generateInvokerClass(origClass, method);
                classes.put(method.getName(), invokerClass);
            } catch (DynamicCodeGenException e) {
                throw e;
            } catch (NotFoundException e) {
                throw new DynamicCodeGenException(e.getMessage(), e);
            } catch (CannotCompileException e) {
                throw new DynamicCodeGenException(e.getMessage(), e);
            }
        }
        return classes;
    }

    private void checkClassValidation(Class<?> origClass)
            throws DynamicCodeGenException {
        // not public, abstract
        int mod = origClass.getModifiers();
        if ((!Modifier.isPublic(mod)) || Modifier.isAbstract(mod)) {
            throwClassValidationException(origClass,
                    "it must be a public class");
        }
    }

    private static void throwClassValidationException(Class<?> origClass,
            String message) throws DynamicCodeGenException {
        throw new DynamicCodeGenException(message + ": " + origClass.getName());
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

    private void checkMethodValidation(Method method, List<Method> methods)
            throws DynamicCodeGenException {
        // check modifiers (public)
        int mod = method.getModifiers();
        if ((!Modifier.isPublic(mod)) || Modifier.isStatic(mod)
                || method.isBridge() || method.isSynthetic()
                || method.isVarArgs()) {
            throwMethodValidationException(method,
                    "it must be a public non-static method");
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
            String message) throws DynamicCodeGenException {
        throw new DynamicCodeGenException(message + ": " + method.getName());
    }

    private Class<?> generateInvokerClass(Class<?> origClass, Method method)
            throws NotFoundException, CannotCompileException,
            DynamicCodeGenException {
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
        sb.append(CHAR_NAME_UNDERSCORE);
        sb.append(method.getName());
        sb.append(POSTFIX_TYPE_NAME_INVOKER);
        String invokerName = sb.toString();
        CtClass invokerCtClass = pool.makeClass(invokerName);
        invokerCtClass.setModifiers(Modifier.PUBLIC);
        return invokerCtClass;
    }

    private void setInterface(CtClass invokerCtClass) throws NotFoundException {
        CtClass invokerInf = pool.get(Invoker.class.getName());
        invokerCtClass.addInterface(invokerInf);
    }

    private void addField(CtClass invokerCtClass, Class<?> origClass)
            throws CannotCompileException {
        // in this part, a created field is not initialized
        StringBuilder sb = new StringBuilder();
        addPublicFieldDecl(sb, origClass, FIELD_NAME_TARGET);
        insertSemicolon(sb);
        // System.out.println("invoker field: " + sb.toString());
        CtField targetCtField = CtField.make(sb.toString(), invokerCtClass);
        invokerCtClass.addField(targetCtField);
    }

    private void addConstructor(CtClass invokerCtClass, Class<?> origClass)
            throws CannotCompileException {
        StringBuilder sb = new StringBuilder();
        sb.append(KEYWORD_MODIFIER_PUBLIC);
        sb.append(CHAR_NAME_SPACE);
        sb.append(invokerCtClass.getSimpleName());
        sb.append(CHAR_NAME_LEFT_PARENTHESIS);
        sb.append(origClass.getName());
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_TARGET);
        sb.append(CHAR_NAME_RIGHT_PARENTHESIS);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        sb.append(FIELD_NAME_TARGET);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_EQUAL);
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_TARGET);
        insertSemicolon(sb);
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
        // System.out.println("invoker constructor: " + sb.toString());
        CtConstructor newCtConstructor = CtNewConstructor.make(sb.toString(),
                invokerCtClass);
        invokerCtClass.addConstructor(newCtConstructor);
    }

    private void addInvokeMethod(CtClass invokerCtClass, Method method)
            throws CannotCompileException, DynamicCodeGenException {
        StringBuilder sb = new StringBuilder();
        sb.append(KEYWORD_MODIFIER_PUBLIC);
        sb.append(CHAR_NAME_SPACE);
        sb.append(void.class.getName()); // return type
        sb.append(CHAR_NAME_SPACE);
        sb.append(METHOD_NAME_INVOKE); // method name
        sb.append(CHAR_NAME_LEFT_PARENTHESIS);
        sb.append(Request.class.getName()); // params
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_REQUEST);
        sb.append(CHAR_NAME_RIGHT_PARENTHESIS);
        sb.append(CHAR_NAME_SPACE);
        insertInvokeMethodBody(sb, method);
        System.out.println("invoker method: " + sb.toString());
        CtMethod newCtMethod = CtNewMethod.make(sb.toString(), invokerCtClass);
        invokerCtClass.addMethod(newCtMethod);
    }

    private void insertInvokeMethodBody(StringBuilder sb, Method m)
            throws DynamicCodeGenException {
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        insertGetArgumentsCall(sb);
        insertTypeConvOfLocalVars(sb, m);
        insertErrorAndRetDecls(sb);
        insertUserDefinedMethodCall(sb, m);
        insertSendResponseCall(sb);
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
    }

    private void insertGetArgumentsCall(StringBuilder sb) {
        // MessagePackObject[] packObjs = req.getArguments();
        sb.append(MessagePackObject.class.getName());
        sb.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        sb.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_EQUAL);
        sb.append(CHAR_NAME_SPACE);
        insertMethodCall(sb, VARIABLE_NAME_REQUEST, METHOD_NAME_GETARGUMENTS,
                new String[0]);
        insertSemicolon(sb);
    }

    private void insertTypeConvOfLocalVars(StringBuilder sb, Method m)
            throws DynamicCodeGenException {
        Class<?>[] types = m.getParameterTypes();
        int j = 0;
        for (int i = 0; i < types.length; ++i) {
            if (!types[i].equals(Request.class)) {
                insertTypeConvOfLocalVar(sb, j, m, types[i], null);
                ++j;
            }
        }
    }

    private void insertTypeConvOfLocalVar(StringBuilder sb, int i, Method m,
            Class<?> c, String n) throws DynamicCodeGenException {
        if (c.isPrimitive()) {
            // primitive type
            insertTypeConvOfLocalVarForPrimTypes(sb, i, m, c, n);
        } else {
            // reference type
            if (c.equals(Boolean.class) || c.equals(Byte.class)
                    || c.equals(Short.class) || c.equals(Integer.class)
                    || c.equals(Float.class) || c.equals(Long.class)
                    || c.equals(Double.class)) {
                // wrapper type
                insertTypeConvOfLocalVarForWrapTypes(sb, i, m, c, n);
            } else if (c.equals(String.class) || c.equals(byte[].class)
                    || c.equals(BigInteger.class)) {
                insertTypeConvOfLocalVarForPrimTypes(sb, i, m, c, n);
            } else if (List.class.isAssignableFrom(c)) {
                ParameterizedType pt = (ParameterizedType) m
                        .getGenericParameterTypes()[i];
                Class<?> vc = (Class<?>) pt.getActualTypeArguments()[0];
                insertTypeConvOfLocalVarForListType(sb, i, c, vc);
            } else if (Map.class.isAssignableFrom(c)) {
                ParameterizedType pt = (ParameterizedType) m
                        .getGenericParameterTypes()[i];
                Class<?> kc = (Class<?>) pt.getActualTypeArguments()[0];
                Class<?> vc = (Class<?>) pt.getActualTypeArguments()[1];
                insertTypeConvOfLocalVarForMapType(sb, i, c, kc, vc);
            } else if (CustomConverter.isRegistered(c)) {
                insertTypeConvOfLocalVarEnhancedTypes(sb, i, c);
            } else if (MessageConvertable.class.isAssignableFrom(c)) {
                insertTypeConvOfLocalVarForMsgConvtblType(sb, i, c);
            } else {
                throw new DynamicCodeGenException("Type error: " + c.getName());
            }
        }
    }

    private void insertTypeConvOfLocalVarForPrimTypes(StringBuilder sb, int i,
            Method m, Class<?> c, String n) throws DynamicCodeGenException {
        // int _$$_0 = packObjs[0].asInt();
        StringBuilder mc = new StringBuilder();
        if (m != null) {
            insertLocalVariableDecl(sb, c, VARIABLE_NAME_ARGS + i);
            sb.append(CHAR_NAME_SPACE);
            sb.append(CHAR_NAME_EQUAL);
            sb.append(CHAR_NAME_SPACE);
            mc.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
            mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
            mc.append(i);
            mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        } else {
            mc.append(n);
        }
        insertMethodCall(sb, mc.toString(), getAsMethodName(c), new String[0]);
        if (m != null) {
            insertSemicolon(sb);
        }
    }

    private void insertTypeConvOfLocalVarForWrapTypes(StringBuilder sb, int i,
            Method m, Class<?> c, String n) throws DynamicCodeGenException {
        StringBuilder mc = null;
        // Integer _$$_0 = new Integer(packObjs[0].intValue());
        if (m != null) {
            insertLocalVariableDecl(sb, c, VARIABLE_NAME_ARGS + i);
            sb.append(CHAR_NAME_SPACE);
            sb.append(CHAR_NAME_EQUAL);
            sb.append(CHAR_NAME_SPACE);
        }
        mc = new StringBuilder();
        if (m != null) {
            mc.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
            mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
            mc.append(i);
            mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        } else {
            mc.append(n);
        }
        String tname = mc.toString();
        mc = new StringBuilder();
        insertMethodCall(mc, tname, getAsMethodName(c), new String[0]);
        insertConsCall(sb, c, mc.toString());
        if (m != null) {
            insertSemicolon(sb);
        }
    }

    private void insertTypeConvOfLocalVarEnhancedTypes(StringBuilder sb, int i,
            Class<?> c) {
        // Foo _$$_0 = new Foo_$$_Enhanced();
        // ((MessageConvertable)_$$_0).messageConvert(packObjs[0]);
        // c = PackUnpackUtil.getEnhancedClass(c);
        insertTypeConvOfLocalVarForMsgConvtblType(sb, i, c);
    }

    private void insertTypeConvOfLocalVarForMsgConvtblType(StringBuilder sb,
            int i, Class<?> c) {
        StringBuilder mc;

        // Foo _$$_0 = new Foo();
        // ((MessageConvertable)_$$_0).messageConvert(packObjs[0]);
        insertLocalVariableDecl(sb, c, VARIABLE_NAME_ARGS + i);
        mc = new StringBuilder();
        insertDefaultConsCall(mc, c);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        mc = new StringBuilder();
        insertTypeCast(mc, MessageConvertable.class, VARIABLE_NAME_ARGS + i);
        String tname = mc.toString();
        mc = new StringBuilder();
        mc.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
        mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        mc.append(i);
        mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        String[] anames = new String[] { mc.toString() };
        insertMethodCall(sb, tname, METHOD_NAME_MSGCONVERT, anames);
        insertSemicolon(sb);
    }

    private void insertTypeConvOfLocalVarForListType(StringBuilder sb, int i,
            Class<?> c, Class<?> gc) throws DynamicCodeGenException {
        StringBuilder mc;

        // List<MessagePackObject> _$$_list = packObjs[0].asList();
        insertLocalVariableDecl(sb, List.class, VARIABLE_NAME_LIST);
        mc = new StringBuilder();
        mc.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
        mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        mc.append(i);
        mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        String tname = mc.toString();
        mc = new StringBuilder();
        insertMethodCall(mc, tname, METHOD_NAME_ASLIST, new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // int size = _$$_list.size();
        insertLocalVariableDecl(sb, int.class, VARIABLE_NAME_SIZE);
        mc = new StringBuilder();
        insertMethodCall(mc, VARIABLE_NAME_LIST, METHOD_NAME_SIZE,
                new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // List<T> _$$_0 = new ArrayList<T>();
        insertLocalVariableDecl(sb, List.class, VARIABLE_NAME_ARGS + i);
        mc = new StringBuilder();
        insertDefaultConsCall(mc, ArrayList.class);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // for loop
        sb.append(KEYWORD_FOR);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_LEFT_PARENTHESIS);
        sb.append(int.class.getName());
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_I);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_EQUAL);
        sb.append(CHAR_NAME_SPACE);
        sb.append(0);
        insertSemicolon(sb);
        sb.append(VARIABLE_NAME_I);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_LESSTHAN);
        sb.append(CHAR_NAME_SPACE);
        sb.append(VARIABLE_NAME_SIZE);
        insertSemicolon(sb);
        sb.append(CHAR_NAME_PLUS);
        sb.append(CHAR_NAME_PLUS);
        sb.append(VARIABLE_NAME_I);
        sb.append(CHAR_NAME_RIGHT_PARENTHESIS);
        sb.append(CHAR_NAME_SPACE);

        // block begin
        // MessagePackObject mpo = _$$_list.get(i);
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        insertLocalVariableDecl(sb, MessagePackObject.class, VARIABLE_NAME_VAL);
        mc = new StringBuilder();
        insertTypeCast(mc, MessagePackObject.class);
        insertMethodCall(mc, VARIABLE_NAME_LIST, METHOD_NAME_GET,
                new String[] { VARIABLE_NAME_I });
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // _$$_0.add(mpo.intValue());
        mc = new StringBuilder();
        insertTypeConvOfLocalVar(mc, -1, null, gc, VARIABLE_NAME_VAL);
        insertMethodCall(sb, VARIABLE_NAME_ARGS + i, METHOD_NAME_ADD,
                new String[] { mc.toString() });
        insertSemicolon(sb);

        // block end
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
    }

    private void insertTypeConvOfLocalVarForMapType(StringBuilder sb, int i,
            Class<?> c, Class<?> gkc, Class<?> gvc)
            throws DynamicCodeGenException {
        StringBuilder mc;

        // Map _$$_map = packObjs[0].asMap();
        insertLocalVariableDecl(sb, Map.class, VARIABLE_NAME_MAP);
        mc = new StringBuilder();
        mc.append(VARIABLE_NAME_MESSAGEPACKOBJECTS);
        mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        mc.append(i);
        mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        String tname = mc.toString();
        mc = new StringBuilder();
        insertMethodCall(mc, tname, METHOD_NAME_ASMAP, new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // Iterator _$$_iter = _$$_map.keySet().iterator();
        insertLocalVariableDecl(sb, Iterator.class, VARIABLE_NAME_ITER);
        mc = new StringBuilder();
        insertMethodCall(mc, VARIABLE_NAME_MAP, METHOD_NAME_KEYSET,
                new String[0]);
        String expr = mc.toString();
        mc = new StringBuilder();
        insertMethodCall(mc, expr, METHOD_NAME_ITERATOR, new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // Map _$$_0 = new HashMap();
        insertLocalVariableDecl(sb, Map.class, VARIABLE_NAME_ARGS + i);
        mc = new StringBuilder();
        insertDefaultConsCall(mc, HashMap.class);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // for loop
        sb.append(KEYWORD_FOR);
        sb.append(CHAR_NAME_SPACE);
        sb.append(CHAR_NAME_LEFT_PARENTHESIS);
        insertSemicolon(sb);
        insertMethodCall(sb, VARIABLE_NAME_ITER, METHOD_NAME_HASNEXT,
                new String[0]);
        insertSemicolon(sb);
        sb.append(CHAR_NAME_RIGHT_PARENTHESIS);
        sb.append(CHAR_NAME_SPACE);

        // block begin
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        // MessagePackObject _$$_key = _$$_iter.next();
        // MessagePackObject _$$_val = _$$_map.get(_$$_key);
        insertLocalVariableDecl(sb, MessagePackObject.class, VARIABLE_NAME_KEY);
        mc = new StringBuilder();
        insertTypeCast(mc, MessagePackObject.class);
        insertMethodCall(mc, VARIABLE_NAME_ITER, METHOD_NAME_NEXT,
                new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);
        insertLocalVariableDecl(sb, MessagePackObject.class, VARIABLE_NAME_VAL);
        mc = new StringBuilder();
        insertTypeCast(mc, MessagePackObject.class);
        insertMethodCall(mc, VARIABLE_NAME_MAP, METHOD_NAME_GET,
                new String[] { VARIABLE_NAME_KEY });
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);

        // _$$_0.add(_$$_key.intValue(), _$$_val.intValue());
        mc = new StringBuilder();
        insertTypeConvOfLocalVar(mc, -1, null, gkc, VARIABLE_NAME_KEY);
        String gkcName = mc.toString();
        mc = new StringBuilder();
        insertTypeConvOfLocalVar(mc, -1, null, gvc, VARIABLE_NAME_VAL);
        String gvcName = mc.toString();
        insertMethodCall(sb, VARIABLE_NAME_ARGS + i, METHOD_NAME_PUT,
                new String[] { gkcName, gvcName });
        insertSemicolon(sb);

        // block end
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
    }

    private void insertErrorAndRetDecls(StringBuilder sb) {
        // Throwable err = null; Object ret = null;
        insertLocalVariableDecl(sb, Throwable.class, VARIABLE_NAME_ERROR);
        insertValueInsertion(sb, KEYWORD_NULL);
        insertSemicolon(sb);
        insertLocalVariableDecl(sb, Object.class, VARIABLE_NAME_RET);
        insertValueInsertion(sb, KEYWORD_NULL);
        insertSemicolon(sb);
    }

    private void insertUserDefinedMethodCall(StringBuilder sb, Method m)
            throws DynamicCodeGenException {
        // try { } catch (Throwable t0) { t = t0; }

        // ret = new Integer(handler.m0(i, j));
        StringBuilder tb = new StringBuilder();
        if (!m.getReturnType().equals(void.class)) {
            tb.append(VARIABLE_NAME_RET);
            tb.append(CHAR_NAME_SPACE);
            tb.append(CHAR_NAME_EQUAL);
            tb.append(CHAR_NAME_SPACE);
        }
        StringBuilder mc = new StringBuilder();
        Class<?>[] paramTypes = m.getParameterTypes();
        String[] anames = new String[paramTypes.length];
        int j = 0;
        for (int i = 0; i < paramTypes.length; ++i) {
            if (paramTypes[i].equals(Request.class)) {
                anames[i] = VARIABLE_NAME_REQUEST;
            } else {
                anames[i] = VARIABLE_NAME_ARGS + j;
                ++j;
            }
        }
        insertMethodCall(mc, FIELD_NAME_TARGET, m.getName(), anames);
        insertTypeConvOfRetType(tb, mc.toString(), m.getReturnType());
        insertSemicolon(tb);

        List<Class<?>> exceptTypes = new ArrayList<Class<?>>();
        exceptTypes.add(Throwable.class);
        List<String> names = new ArrayList<String>();
        names.add(VARIABLE_NAME_ERROR + 0);
        StringBuilder tcb = new StringBuilder();
        tcb.append(VARIABLE_NAME_ERROR);
        insertValueInsertion(tcb, VARIABLE_NAME_ERROR + 0);
        insertSemicolon(tcb);
        List<String> catches = new ArrayList<String>();
        catches.add(tcb.toString());
        insertTryCatchBlocks(sb, tb.toString(), exceptTypes, names, catches);
    }

    private void insertTypeConvOfRetType(StringBuilder sb, String expr,
            Class<?> type) throws DynamicCodeGenException {
        if (type.equals(void.class)) { // void
            sb.append(expr);
        } else {
            insertTypeConvToObjectType(sb, type, expr);
        }
    }

    private void insertSendResponseCall(StringBuilder sb) {
        // req.sendResponse(ret, err);
        insertMethodCall(sb, VARIABLE_NAME_REQUEST, METHOD_NAME_SENDRESPONSE,
                new String[] { VARIABLE_NAME_RET, VARIABLE_NAME_ERROR });
        insertSemicolon(sb);
    }

    private Class<?> createClass(CtClass invokerCtClass)
            throws CannotCompileException {
        return invokerCtClass.toClass(null, null);
    }
}
