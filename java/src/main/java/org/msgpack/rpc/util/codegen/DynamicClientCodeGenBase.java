package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.concurrent.ConcurrentHashMap;

import javassist.CannotCompileException;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtNewConstructor;
import javassist.NotFoundException;

import org.msgpack.Template;
import org.msgpack.rpc.Client;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class DynamicClientCodeGenBase extends DynamicRPCCodeGenBase {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicClientCodeGenBase.class);

    public static interface ClientAccessor {
        void setClient(Client _$$_c);
    }

    public static class ClientTemplateTemplate extends TemplateTemplate
            implements ClientAccessor {
        public Client _$$_client;

        public void setClient(Client _$$_c) {
            _$$_client = _$$_c;
        }
    }

    private ConcurrentHashMap<String, Class<?>> classCache;

    private ConcurrentHashMap<String, Template[]> tmplsCache;

    DynamicClientCodeGenBase() {
        super();
        classCache = new ConcurrentHashMap<String, Class<?>>();
        tmplsCache = new ConcurrentHashMap<String, Template[]>();
    }

    public Class<?> getCache(String handlerName) {
        return classCache.get(handlerName);
    }

    public void setCache(String handlerName, Class<?> clientClass) {
        classCache.putIfAbsent(handlerName, clientClass);
    }

    public Template[] getTemplates(String handlerName) {
        return tmplsCache.get(handlerName);
    }

    public void setTemplates(String origName, Template[] tmpls) {
        tmplsCache.putIfAbsent(origName, tmpls);
    }

    public Class<?> generateClientClass(String handlerName, Class<?> handlerType) {
        try {
            LOG.debug("start generating a client program for " + handlerType);
            checkTypeValidation(handlerType);
            Method[] methods = getDeclaredMethods(handlerType, true);
            Template[] tmpls = createReturnTypeTemplates(methods);
            setTemplates(handlerName, tmpls);
            CtClass clientCtClass = makeClass(handlerName);
            setSuperclass(clientCtClass, ClientTemplateTemplate.class);
            setInterface(clientCtClass, handlerType);
            addDefaultConstructor(clientCtClass);
            addHandlerMethods(clientCtClass, methods);
            Class<?> clientClass = createClass(clientCtClass);
            LOG.debug("generated a client program: " + clientCtClass.getName());
            return clientClass;
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

    @Override
    protected void checkTypeValidation(Class<?> handlerType) {
        // modifiers
        int mod = handlerType.getModifiers();
        if ((!Modifier.isPublic(mod))) {
            throwTypeValidationException(handlerType,
                    "it must be a public interface type");
        }

        // interface type
        if (!handlerType.isInterface()) {
            throwTypeValidationException(handlerType,
                    "it must be a public interface type");
        }
    }

    protected void addDefaultConstructor(CtClass newCtClass)
            throws CannotCompileException {
        CtConstructor cons = CtNewConstructor.defaultConstructor(newCtClass);
        newCtClass.addConstructor(cons);
    }

    protected void addHandlerMethods(CtClass newCtClass, Method[] methods) {
        for (int i = 0; i < methods.length; ++i) {
            addHandlerMethod(newCtClass, methods[i], i);
        }
    }

    protected void addHandlerMethod(CtClass newCtClass, Method method, int i) {
        DynamicCodeGenException e = new DynamicCodeGenException("Fatal error: "
                + this.getClass().getName());
        LOG.error(e.getMessage(), e);
        throw e;
    }

    protected Object newClientInstance(Class<?> clientClass, Client client,
            String handlerName) {
        LOG.trace("create a new object of type: " + clientClass.getName());
        try {
            Object obj = clientClass.newInstance();
            Template[] tmpls = getTemplates(handlerName);
            ((DynamicClientCodeGenBase.ClientAccessor) obj).setClient(client);
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
