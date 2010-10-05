package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Constructor;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.msgpack.Template;
import org.msgpack.rpc.Dispatcher;
import org.msgpack.rpc.Request;
import org.msgpack.rpc.util.codegen.DynamicInvokersGen;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.msgpack.util.codegen.DynamicCodeGenBase.TemplateAccessor;

public class DynamicCodeGenDispatcher implements Dispatcher {

    public interface Invoker {
        void invoke(Request reqest);
    }

    private static DynamicInvokersGen gen;

    private Map<String, Invoker> invokersCache = new ConcurrentHashMap<String, Invoker>();

    private Invoker getCache(String methodName) {
        return invokersCache.get(methodName);
    }

    private void setCache(String methodName, Invoker invoker) {
        if (invoker != null) {
            invokersCache.put(methodName, invoker);
        }
    }

    public DynamicCodeGenDispatcher(Object origObj)
            throws DynamicCodeGenException {
        if (gen == null) {
            gen = new DynamicInvokersGen();
        }

        Class<?> origClass = origObj.getClass();
        String origName = origClass.getName();
        Map<String, Class<?>> classCache = null;
        try {
            if ((classCache = gen.getCache(origName)) == null) {
                // generate invoker classes related to the original class
                classCache = gen.generateInvokerClasses(origObj, origClass);
                // set the generated invoker classes to a cache
                gen.setCache(origName, classCache);
            }
            // create a new invoker object
            if (classCache != null) {
                newInvokerInstances(origObj, origClass, classCache);
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