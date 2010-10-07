package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import javassist.CannotCompileException;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtNewConstructor;
import javassist.CtNewMethod;
import javassist.NotFoundException;

import org.msgpack.Template;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.Request;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class DynamicSyncClientGen extends DynamicInvokersGen {
    public static interface ClientAccessor {
        void setClient(Client _$$_c);
    }

    public static class ClientAccessorImpl implements ClientAccessor {
        public Client _$$_client;

        public void setClient(Client _$$_c) {
            _$$_client = _$$_c;
        }
    }

    private static Logger LOG = LoggerFactory.getLogger(DynamicSyncClientGen.class);

    private static AtomicInteger COUNTER = new AtomicInteger(0);

    private static int inc() {
        return COUNTER.addAndGet(1);
    }

    private ConcurrentHashMap<String, Class<?>> classCache;

    private ConcurrentHashMap<String, Template[]> tmplsCache;

    public DynamicSyncClientGen() {
        super();
        classCache = new ConcurrentHashMap<String, Class<?>>();
        tmplsCache = new ConcurrentHashMap<String, Template[]>();
    }

    public Class<?> getClientClassCache(String handlerName) {
        return classCache.get(handlerName);
    }

    public void setClientClassCache(String handlerName, Class<?> clientClass) {
        classCache.putIfAbsent(handlerName, clientClass);
    }

    public Template[] getReturnTypeTemplates(String handlerName) {
        return tmplsCache.get(handlerName);
    }

    public void setReturnTypeTemplates(String origName, Template[] tmpls) {
        tmplsCache.putIfAbsent(origName, tmpls);
    }

    public Class<?> generateClientClass(String handlerName, Class<?> handlerType) {
        try {
            checkTypeValidation(handlerType);
            Method[] methods = getDeclaredMethods(handlerType, true);
            Template[] tmpls = createReturnTypeTemplates(methods);
            setReturnTypeTemplates(handlerName, tmpls);
            CtClass clientCtClass = makeClass(handlerName);
            setInterface(clientCtClass, TemplateAccessor.class);
            setInterface(clientCtClass, ClientAccessor.class);
            setInterface(clientCtClass, handlerType);
            addDefaultConstructor(clientCtClass);
            addTemplateArrayField(clientCtClass);
            addSetTemplatesMethod(clientCtClass);
            addClientField(clientCtClass);
            addSetClientMethod(clientCtClass);
            addHandlerMethods(clientCtClass, methods);
            return createClass(clientCtClass);
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

    protected void checkTypeValidation(Class<?> handlerType) {
        // modifiers
        int mod = handlerType.getModifiers();
        if ((!Modifier.isPublic(mod))) {
            throwClassValidationException(handlerType,
                    "it must be a public interface type");
        }

        // interface type
        if (!handlerType.isInterface()) {
            throwClassValidationException(handlerType,
                    "it must be a public interface type");
        }
    }

    protected Template[] createReturnTypeTemplates(Method[] methods) {
        Template[] tmpls = new Template[methods.length];
        for (int i = 0; i < tmpls.length; ++i) {
            tmpls[i] = createReturnTypeTemplate(methods[i]);
        }
        return tmpls;
    }

    private Template createReturnTypeTemplate(Method method) {
        Type t = method.getGenericReturnType();
        Class<?> c = method.getReturnType();
        if (List.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c)) {
            return createTemplate(t);
        } else if (c.equals(Request.class)) {
            return null;
        } else {
            return createTemplate(c);
        }
    }

    private CtClass makeClass(String handlerName) throws NotFoundException {
        StringBuilder sb = new StringBuilder();
        sb.append(handlerName);
        sb.append(POSTFIX_TYPE_NAME_CLIENT);
        sb.append(inc());
        String invokerName = sb.toString();
        CtClass newCtClass = pool.makeClass(invokerName);
        newCtClass.setModifiers(Modifier.PUBLIC);
        return newCtClass;
    }

    private void addDefaultConstructor(CtClass newCtClass)
            throws CannotCompileException {
        CtConstructor cons = CtNewConstructor.defaultConstructor(newCtClass);
        newCtClass.addConstructor(cons);
    }

    protected void addClientField(CtClass newCtClass) throws NotFoundException,
            CannotCompileException {
        CtClass acsCtClass = pool.get(ClientAccessorImpl.class.getName());
        CtField clientField = acsCtClass.getDeclaredField(VARIABLE_NAME_CLIENT);
        CtField clientField2 = new CtField(clientField.getType(), clientField
                .getName(), newCtClass);
        newCtClass.addField(clientField2);
    }

    protected void addSetClientMethod(CtClass newCtClass)
            throws NotFoundException, CannotCompileException {
        CtClass acsCtClass = pool.get(ClientAccessorImpl.class.getName());
        CtMethod setclientMethod = acsCtClass
                .getDeclaredMethod(METHOD_NAME_SETCLIENT);
        CtMethod setclientMethod2 = CtNewMethod.copy(setclientMethod,
                newCtClass, null);
        newCtClass.addMethod(setclientMethod2);
    }

    private void addHandlerMethods(CtClass newCtClass, Method[] methods) {
        for (int i = 0; i < methods.length; ++i) {
            addHandlerMethod(newCtClass, methods[i], i);
        }
    }

    private void addHandlerMethod(CtClass newCtClass, Method method, int index) {
        StringBuilder sb = new StringBuilder();
        insertHandlerMethodBody(sb, method, index);
        LOG.trace("handler method body src: " + sb.toString());
        try {
            int mod = javassist.Modifier.PUBLIC;
            CtClass returnType = classToCtClass(method.getReturnType());
            String mname = method.getName();
            Class<?>[] paramTypes0 = method.getParameterTypes();
            CtClass[] paramTypes = new CtClass[paramTypes0.length];
            for (int i = 0; i < paramTypes.length; ++i) {
                paramTypes[i] = classToCtClass(paramTypes0[i]);
            }
            Class<?>[] exceptTypes0 = method.getExceptionTypes();
            CtClass[] exceptTypes = new CtClass[exceptTypes0.length];
            for (int i = 0; i < exceptTypes.length; ++i) {
                exceptTypes[i] = classToCtClass(exceptTypes0[i]);
            }
            CtMethod newCtMethod = CtNewMethod.make(mod, returnType, mname,
                    paramTypes, exceptTypes, sb.toString(), newCtClass);
            newCtClass.addMethod(newCtMethod);
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
            throw ex;
        }
    }

    private void insertHandlerMethodBody(StringBuilder sb, Method method, int i) {
        // MessagePackObject _$$_mpo = _$$_client.callApply("m0", $args);
        // $_ = ($r)_$$_templates[i].convert(_$$_mpo);
        sb.append("{ ");
        Class<?> c = method.getReturnType();
        if (!c.equals(void.class)) {
            sb.append("org.msgpack.MessagePackObject _$$_mpo = _$$_client.callApply(\"" + method.getName() + "\", $args); ");
            sb.append("return ($r)_$$_templates[" + i + "].convert(_$$_mpo); ");
        } else {
            sb.append("_$$_client.callApply(\"" + method.getName() + "\", $args); ");
        }
        sb.append("}");
    }

    public Object newClientInstance(Class<?> clientClass, Client client,
            String handlerName) {
        try {
            Object obj = clientClass.newInstance();
            Template[] tmpls = getReturnTypeTemplates(handlerName);
            ((ClientAccessor) obj).setClient(client);
            ((TemplateAccessor) obj).setTemplates(tmpls);
            return obj;
        } catch (Exception e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }
}
