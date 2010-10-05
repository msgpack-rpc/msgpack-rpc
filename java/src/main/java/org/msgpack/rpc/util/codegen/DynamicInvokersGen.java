package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
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
import org.msgpack.Template;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.util.codegen.DynamicCodeGenDispatcher.Invoker;
import org.msgpack.util.codegen.DynamicCodeGenBase;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DynamicInvokersGen extends DynamicCodeGenBase implements Constants {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicInvokersGen.class);

    private ClassPool pool;

    private ConcurrentHashMap<String, Map<String, Class<?>>> classesCache;

    private ConcurrentHashMap<String, Map<String, Template[]>> tmplsCache;

    public DynamicInvokersGen() {
        LOG.info("create an instance of " + DynamicInvokersGen.class.getName());
        pool = ClassPool.getDefault();
        classesCache = new ConcurrentHashMap<String, Map<String, Class<?>>>();
        tmplsCache = new ConcurrentHashMap<String, Map<String, Template[]>>();
    }

    public Map<String, Class<?>> getCache(String origName) {
        return classesCache.get(origName);
    }

    public void setCache(String origName, Map<String, Class<?>> cache) {
        classesCache.putIfAbsent(origName, cache);
    }

    public Template[] getTemplates(String origName, String origMethodName) {
        return tmplsCache.get(origName).get(origMethodName);
    }

    public void setTemplates(String origName, Map<String, Template[]> tmpls) {
        tmplsCache.putIfAbsent(origName, tmpls);
    }

    public Map<String, Class<?>> generateInvokerClasses(Object origObj,
            Class<?> origClass) throws DynamicCodeGenException {
        LOG.debug("generate invokers for a class: " + origClass.getName());
        String origName = origClass.getName();
        Map<String, Class<?>> cache = classesCache.get(origName);
        if (cache != null) {
            return cache;
        }
        Map<String, Class<?>> classes = null;
        try {
            classes = generateInvokerClasses(origName, origClass);
        } catch (DynamicCodeGenException e) {
            LOG.error(e.getMessage(), e);
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
        Map<String, Template[]> tmpls = createTemplates(methods);
        setTemplates(origName, tmpls);
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

    Map<String, Template[]> createTemplates(Method[] methods) {
        Map<String, Template[]> ret = new HashMap<String, Template[]>();
        for (Method method : methods) {
            Template[] tmpls = createTemplates(method);
            ret.put(method.getName(), tmpls);
        }
        return ret;
    }

    Template[] createTemplates(Method method) {
        Type[] paramTypes = method.getGenericParameterTypes();
        Class<?>[] paramTypes2 = method.getParameterTypes();
        // Template[] tmpls = new Template[paramTypes.length];
        List<Template> tmpls = new ArrayList<Template>();
        for (int i = 0; i < paramTypes.length; ++i) {
            Type t = paramTypes[i];
            Class<?> c = paramTypes2[i];
            if (List.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c)) {
                tmpls.add(createTemplate(t));
            } else if (c.equals(Request.class)) {
                ; // ignore
            } else {
                tmpls.add(createTemplate(c));
            }
        }
        return tmpls.toArray(new Template[0]);
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
            throws NotFoundException, CannotCompileException {
        String origName = origClass.getName();
        CtClass invokerCtClass = makeClass(origName, method);
        setInterface(invokerCtClass, Invoker.class);
        setInterface(invokerCtClass, TemplateAccessor.class);
        addTemplateArrayField(invokerCtClass, origClass);
        addTargetField(invokerCtClass, origClass);
        addConstructor(invokerCtClass, origClass);
        addSetTemplatesMethod(invokerCtClass, origClass);
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

    private void setInterface(CtClass packerCtClass, Class<?> infClass)
            throws NotFoundException {
        CtClass infCtClass = pool.get(infClass.getName());
        packerCtClass.addInterface(infCtClass);
    }

    private void addTemplateArrayField(CtClass newCtClass, Class<?> origClass) {
        StringBuilder sb = new StringBuilder();
        addPublicFieldDecl(sb, Template.class, VARIABLE_NAME_TEMPLATES, 1);
        insertSemicolon(sb);
        LOG.trace("templates field src: " + sb.toString());
        try {
            CtField templatesCtField = CtField.make(sb.toString(), newCtClass);
            newCtClass.addField(templatesCtField);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private void addTargetField(CtClass invokerCtClass, Class<?> origClass) {
        // in this part, a created field is not initialized
        StringBuilder sb = new StringBuilder();
        addPublicFieldDecl(sb, origClass, FIELD_NAME_TARGET);
        insertSemicolon(sb);
        LOG.trace("invoker field src: " + sb.toString());
        try {
            CtField targetCtField = CtField.make(sb.toString(), invokerCtClass);
            invokerCtClass.addField(targetCtField);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private void addConstructor(CtClass invokerCtClass, Class<?> origClass) {
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
        insertValueInsertion(sb, VARIABLE_NAME_TARGET);
        insertSemicolon(sb);
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
        LOG.trace("invoker constructor src: " + sb.toString());
        try {
            CtConstructor newCtConstructor = CtNewConstructor.make(sb
                    .toString(), invokerCtClass);
            invokerCtClass.addConstructor(newCtConstructor);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private void addSetTemplatesMethod(CtClass newCtClass, Class<?> origClass) {
        StringBuilder sb = new StringBuilder();
        StringBuilder body = new StringBuilder();
        body.append("_$$_templates = _$$_tmpls;");
        addPublicMethodDecl(sb, METHOD_NAME_SETTEMPLATES, void.class,
                new Class<?>[] { Template.class }, new int[] { 1 },
                new String[] { VARIABLE_NAME_TEMPLATES0 }, new Class<?>[0],
                body.toString());
        LOG.trace("settemplates method src: " + sb.toString());
        try {
            CtMethod newCtMethod = CtNewMethod.make(sb.toString(), newCtClass);
            newCtClass.addMethod(newCtMethod);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private void addInvokeMethod(CtClass invokerCtClass, Method method) {
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
        LOG.trace("invoker method src: " + sb.toString());
        try {
            CtMethod newCtMethod = CtNewMethod.make(sb.toString(),
                    invokerCtClass);
            invokerCtClass.addMethod(newCtMethod);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }

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
        // MessagePackObject packObj = req.getArguments();
        // MessagePackObject[] packObjs = packObj.asArray();
        StringBuilder mc = new StringBuilder();
        insertLocalVariableDecl(sb, MessagePackObject.class, VARIABLE_NAME_MPO);
        insertMethodCall(mc, VARIABLE_NAME_REQUEST, METHOD_NAME_GETARGUMENTS,
                new String[0]);
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);
        mc = new StringBuilder();
        insertLocalVariableDecl(sb, MessagePackObject.class,
                VARIABLE_NAME_MPOS, 1);
        insertMethodCall(mc, VARIABLE_NAME_MPO, METHOD_NAME_ASARRAY,
                new String[0]);
        insertValueInsertion(sb, mc.toString());
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
            Class<?> type, String n) {
        // int _$$_i = ((Integer) _$$_tmpl[i].convert(_$$_mpos[i])).intValue();
        insertLocalVariableDecl(sb, type, VARIABLE_NAME_ARGS + i);
        String castType = null;
        String rawValueGetter = null;
        if (type.isPrimitive()) {
            if (type.equals(byte.class)) {
                castType = "(Byte)";
                rawValueGetter = "byteValue";
            } else if (type.equals(boolean.class)) {
                castType = "(Boolean)";
                rawValueGetter = "booleanValue";
            } else if (type.equals(short.class)) {
                castType = "(Short)";
                rawValueGetter = "shortValue";
            } else if (type.equals(int.class)) {
                castType = "(Integer)";
                rawValueGetter = "intValue";
            } else if (type.equals(long.class)) {
                castType = "(Long)";
                rawValueGetter = "longValue";
            } else if (type.equals(float.class)) {
                castType = "(Float)";
                rawValueGetter = "floatValue";
            } else if (type.equals(double.class)) {
                castType = "(Double)";
                rawValueGetter = "doubleValue";
            } else {
                throw new DynamicCodeGenException("Fatal error: "
                        + type.getName());
            }
        } else if (type.isArray()) {
            Class<?> ct = type.getComponentType();
            if (ct.equals(byte.class)) {
                castType = "(byte[])";
            } else {
                throw new UnsupportedOperationException("Not supported yet: "
                        + type.getName());
            }
        } else {
            castType = "(" + type.getName() + ")";
        }
        StringBuilder mc = new StringBuilder();
        mc.append(castType);
        mc.append(VARIABLE_NAME_TEMPLATES);
        mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        mc.append(i);
        mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        String tname = mc.toString();
        mc = new StringBuilder();
        mc.append(VARIABLE_NAME_MPOS);
        mc.append(CHAR_NAME_LEFT_SQUARE_BRACKET);
        mc.append(i);
        mc.append(CHAR_NAME_RIGHT_SQUARE_BRACKET);
        String aname = mc.toString();
        mc = new StringBuilder();
        insertMethodCall(mc, tname, METHOD_NAME_CONVERT, new String[] { aname });
        if (type.isPrimitive()) {
            tname = mc.toString();
            mc = new StringBuilder();
            mc.append(Constants.CHAR_NAME_LEFT_PARENTHESIS);
            mc.append(tname);
            mc.append(Constants.CHAR_NAME_RIGHT_PARENTHESIS);
            tname = mc.toString();
            mc = new StringBuilder();
            insertMethodCall(mc, tname, rawValueGetter, new String[0]);
        }
        insertValueInsertion(sb, mc.toString());
        insertSemicolon(sb);
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
