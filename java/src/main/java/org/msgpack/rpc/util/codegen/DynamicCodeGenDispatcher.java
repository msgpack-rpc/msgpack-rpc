package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.msgpack.Template;
import org.msgpack.rpc.Dispatcher;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.util.codegen.DynamicInvokersGen;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.msgpack.util.codegen.DynamicCodeGenBase.TemplateAccessor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DynamicCodeGenDispatcher implements Dispatcher {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicCodeGenDispatcher.class);

    public interface Invoker {
        void invoke(Request reqest);
    }

    private static DynamicInvokersGen gen;

    private ConcurrentHashMap<String, Invoker> invokersCache = new ConcurrentHashMap<String, Invoker>();

    private Invoker getCache(String methodName) {
        return invokersCache.get(methodName);
    }

    private void setCache(String methodName, Invoker invoker) {
        if (invoker != null) {
            invokersCache.putIfAbsent(methodName, invoker);
        }
    }

    public DynamicCodeGenDispatcher(Object handler) {
        this(handler.getClass(), false, null, handler);
    }

    public DynamicCodeGenDispatcher(Class<?> handlerType, Object handler)
            throws DynamicCodeGenException {
        this(handler.getClass(), handlerType.isInterface(), null, handler);
    }

    public DynamicCodeGenDispatcher(Method[] handlerMethods, Object handler) {
        this(handler.getClass(), handler.getClass().isInterface(),
                handlerMethods, handler);
    }

    private DynamicCodeGenDispatcher(Class<?> handlerType, boolean isInterface,
            Method[] handlerMethods, Object handler) {
        LOG.info("create an instance of " + this.getClass().getName()
                + ": handler type: " + handlerType.getName());
        if (gen == null) {
            gen = new DynamicInvokersGen();
        }

        String handlerName = handlerType.getName();
        Map<String, Class<?>> classCache = null;
        try {
            if ((classCache = gen.getCache(handlerName)) == null) {
                // generate invoker classes related to the original class
                classCache = gen.generateInvokerClasses(handlerType,
                        isInterface, handlerMethods);
                // set the generated invoker classes to a cache
                gen.setCache(handlerName, classCache);
            }
            // create a new invoker object
            if (classCache != null) {
                newInvokerInstances(handler, handlerType, classCache);
            }
        } catch (DynamicCodeGenException e) {
            throw e;
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
                Template[] tmpls = gen.getTemplates(origClass.getName(),
                        methodName);
                ((TemplateAccessor) invoker).setTemplates(tmpls);
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
            throw new DynamicCodeGenException("Invoker not found: "
                    + request.getMethodName());
        }
        invoker.invoke(request);
    }
}