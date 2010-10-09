package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javassist.CannotCompileException;
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
import org.msgpack.rpc.util.codegen.DynamicDispatcher.Invoker;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class DynamicInvokersCodeGen extends DynamicRPCCodeGenBase {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicInvokersCodeGen.class);

    private ConcurrentHashMap<String, Map<String, Class<?>>> classesCache;

    private ConcurrentHashMap<String, Map<String, Template[]>> tmplsCache;

    public DynamicInvokersCodeGen() {
        super();
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

    public Map<String, Class<?>> generateInvokerClasses(Class<?> handlerType,
            boolean isInterface, Method[] handlerMethods) {
        LOG.debug("start generating invoker classes for handler type: "
                + handlerType.getName());
        if (!isInterface && handlerMethods == null) {
            checkTypeValidation(handlerType);
        }
        Method[] methods;
        if (handlerMethods == null) {
            methods = getDeclaredMethods(handlerType, isInterface);
        } else {
            methods = getValidatedMethods(handlerMethods);
        }
        Map<String, Template[]> tmpls = createMethodParamTypeTemplates(methods);
        setTemplates(handlerType.getName(), tmpls);
        Map<String, Class<?>> classes = new HashMap<String, Class<?>>();
        for (Method method : methods) {
            LOG.debug("start generating invoker class " + handlerType.getName()
                    + " for handler: " + method.getName());
            Class<?> invokerClass = generateInvokerClass(handlerType, method);
            LOG.debug("generated invoker class: " + handlerType.getName()
                    + " for handler: " + method.getName());
            classes.put(method.getName(), invokerClass);
        }
        return classes;
    }

    @Override
    protected void checkTypeValidation(Class<?> type) {
        // not public, abstract
        int mod = type.getModifiers();
        if ((!Modifier.isPublic(mod)) || Modifier.isAbstract(mod)) {
            throwTypeValidationException(type, "it must be a public class");
        }
    }

    private Method[] getValidatedMethods(Method[] methods) {
        ArrayList<Method> allMethods = new ArrayList<Method>();
        for (Method method : methods) {
            try {
                checkMethodValidation(method, allMethods, false);
                allMethods.add(method);
            } catch (Exception e) { // ignore
                LOG.trace(e.getMessage(), e);
            }
        }
        return allMethods.toArray(new Method[0]);
    }

    private Class<?> generateInvokerClass(Class<?> origClass, Method method) {
        try {
            String origName = origClass.getName();
            CtClass invokerCtClass = makeClass(origName, method);
            setInterface(invokerCtClass, Invoker.class);
            setInterface(invokerCtClass, TemplateAccessor.class);
            addTargetField(invokerCtClass, origClass);
            addTemplateArrayField(invokerCtClass);
            addSetTemplatesMethod(invokerCtClass);
            addConstructor(invokerCtClass, origClass);
            addInvokeMethod(invokerCtClass, method);
            return createClass(invokerCtClass);
        } catch (NotFoundException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private CtClass makeClass(String origName, Method method)
            throws NotFoundException {
        StringBuilder sb = new StringBuilder();
        sb.append(origName);
        sb.append(CHAR_NAME_UNDERSCORE);
        sb.append(method.getName());
        sb.append(POSTFIX_TYPE_NAME_INVOKER);
        sb.append(inc());
        String invokerName = sb.toString();
        CtClass invokerCtClass = pool.makeClass(invokerName);
        invokerCtClass.setModifiers(Modifier.PUBLIC);
        return invokerCtClass;
    }

    private void addTargetField(CtClass invokerCtClass, Class<?> origClass) {
        // in this part, a created field is not initialized
        StringBuilder sb = new StringBuilder();
        Object[] args = new Object[] { origClass.getName() };
        sb.append(String.format(STATEMENT_INVOKERS_TARGETFIELD_01, args));
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
        Object[] args = new Object[0];
        sb.append(String.format(STATEMENT_INVOKERS_CONSTRUCTORBODY_01, args));
        LOG.trace("invoker constructor src: " + sb.toString());
        try {
            CtClass[] paramTypes = new CtClass[] { classToCtClass(origClass) };
            CtClass[] exceptTypes = new CtClass[0];
            CtConstructor newCtCons = CtNewConstructor.make(paramTypes,
                    exceptTypes, sb.toString(), invokerCtClass);
            invokerCtClass.addConstructor(newCtCons);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        } catch (NotFoundException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
        }
    }

    private void addInvokeMethod(CtClass invokerCtClass, Method method) {
        StringBuilder sb = new StringBuilder();
        insertInvokeMethodBody(sb, method);
        LOG.trace("invoker method src: " + sb.toString());
        try {
            int mod = javassist.Modifier.PUBLIC;
            CtClass returnType = classToCtClass(void.class);
            String mname = METHOD_NAME_INVOKE;
            CtClass[] paramTypes = new CtClass[] { classToCtClass(Request.class) };
            CtClass[] exceptTypes = new CtClass[0];
            CtMethod newCtMethod = CtNewMethod.make(mod, returnType, mname,
                    paramTypes, exceptTypes, sb.toString(), invokerCtClass);
            invokerCtClass.addMethod(newCtMethod);
        } catch (CannotCompileException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        } catch (NotFoundException e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
        }
    }

    private void insertInvokeMethodBody(StringBuilder sb, Method m)
            throws DynamicCodeGenException {
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        // MessagePackObject _$$_mpo = _$$_req.getArguments();
        Object[] args0 = new Object[] { classToString(MessagePackObject.class) };
        sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_01, args0));
        // MessagePackObject[] _$$_mpos = _$$_mpo.asArray();
        Object[] args1 = new Object[] { classToString(MessagePackObject[].class) };
        sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_02, args1));
        insertTypeConvOfLocalVars(sb, m);
        // Throwable _$$_err = null; Object _$$_ret = null;
        Object[] args2 = new Object[] { Throwable.class.getName(),
                Object.class.getName() };
        sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_06, args2));
        insertUserDefinedMethodCall(sb, m);
        // _$$_req.sendResponse(_$$_ret, _$$_err);
        Object[] args3 = new Object[0];
        sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_09, args3));
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
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
        Object[] args0 = new Object[] { i, i };
        String s = String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_03, args0);
        String typeName = classToString(type);
        if (type.isPrimitive()) {
            String mname = getPrimTypeValueMethodName(type);
            String wrap = getPrimToWrapperType(type).getName();
            Object[] args1 = new Object[] { typeName, i, wrap, s, mname };
            sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_04,
                    args1));
        } else {
            Object[] args1 = new Object[] { typeName, i, typeName, s };
            sb.append(String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_05,
                    args1));
        }
    }

    private void insertUserDefinedMethodCall(StringBuilder sb, Method m)
            throws DynamicCodeGenException {
        StringBuilder ab = new StringBuilder();
        Class<?>[] paramTypes = m.getParameterTypes();
        int j = 0;
        for (int i = 0; i < paramTypes.length; ++i) {
            if (paramTypes[i].equals(Request.class)) {
                ab.append("$1");
            } else {
                ab.append(VARIABLE_NAME_ARGS + j);
                ++j;
            }
            if (i + 1 != paramTypes.length) {
                ab.append(CHAR_NAME_COMMA);
                ab.append(CHAR_NAME_SPACE);
            }
        }
        Class<?> returnType = m.getReturnType();
        boolean isVoid = returnType.equals(void.class);
        boolean isPrim = (!isVoid) && returnType.isPrimitive();
        Object[] args0 = new Object[] {
                isVoid ? "" : "_$$_ret =",
                isPrim ? "new " + getPrimToWrapperType(returnType).getName()
                        + "(" : "", m.getName(), ab.toString(),
                isPrim ? ")" : "", };
        String s = String.format(STATEMENT_INVOKERS_INVOKEMETHODBODY_07, args0);
        Object[] args1 = new Object[] { s, Throwable.class.getName() };
        sb.append(String.format(
                Constants.STATEMENT_INVOKERS_INVOKEMETHODBODY_08, args1));
    }
}
