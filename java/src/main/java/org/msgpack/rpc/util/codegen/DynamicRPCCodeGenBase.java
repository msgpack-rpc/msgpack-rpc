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
package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.msgpack.Template;
import org.msgpack.rpc.Request;
import org.msgpack.util.codegen.DynamicCodeGenBase;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class DynamicRPCCodeGenBase extends DynamicCodeGenBase implements Constants {

    private static Logger LOG = LoggerFactory
            .getLogger(DynamicRPCCodeGenBase.class);

    protected DynamicRPCCodeGenBase() {
        super();
    }

    protected Method[] getDeclaredMethods(Class<?> handlerType,
            boolean isInterface) {
        ArrayList<Method> allMethods = new ArrayList<Method>();
        Class<?> nextType = handlerType;
        while (nextType != null && !nextType.equals(Object.class)) {
            Method[] methods = nextType.getDeclaredMethods();
            for (Method method : methods) {
                try {
                    checkMethodValidation(method, allMethods, isInterface);
                    allMethods.add(method);
                } catch (Exception e) { // ignore
                    LOG.trace(e.getMessage(), e);
                }
            }
            nextType = nextType.getSuperclass();
        }
        return allMethods.toArray(new Method[0]);
    }

    protected void checkMethodValidation(Method method, List<Method> methods,
            boolean isInterface) throws DynamicCodeGenException {
        // check modifiers
        int mod = method.getModifiers();
        if ((!Modifier.isPublic(mod)) || Modifier.isStatic(mod)
                || method.isBridge() || method.isSynthetic()
                || method.isVarArgs()) {
            throwMethodValidationException(method,
                    "it must be a public non-static method");
        }
        if (!isInterface) {
            if (Modifier.isAbstract(mod)) {
                throwMethodValidationException(method,
                        "it must not be an abstract method");
            }
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

    protected Map<String, Template[]> createMethodParamTypeTemplates(
            Method[] methods) {
        Map<String, Template[]> ret = new HashMap<String, Template[]>();
        for (Method method : methods) {
            Template[] tmpls = createMethodParamTypeTemplates(method);
            ret.put(method.getName(), tmpls);
        }
        return ret;
    }

    protected Template[] createMethodParamTypeTemplates(Method method) {
        Type[] paramTypes = method.getGenericParameterTypes();
        Class<?>[] paramTypes2 = method.getParameterTypes();
        // Template[] tmpls = new Template[paramTypes.length];
        List<Template> tmpls = new ArrayList<Template>();
        for (int i = 0; i < paramTypes.length; ++i) {
            Type t = paramTypes[i];
            Class<?> c = paramTypes2[i];
            if (List.class.isAssignableFrom(c)
                    || Map.class.isAssignableFrom(c)
                    || Collection.class.isAssignableFrom(c)) {
                tmpls.add(createTemplate(t));
            } else if (c.equals(Request.class)) {
                ; // ignore
            } else {
                tmpls.add(createTemplate(c));
            }
        }
        return tmpls.toArray(new Template[0]);
    }

    protected Template[] createReturnTypeTemplates(Method[] methods) {
        Template[] tmpls = new Template[methods.length];
        for (int i = 0; i < tmpls.length; ++i) {
            tmpls[i] = createReturnTypeTemplate(methods[i]);
        }
        return tmpls;
    }

    protected Template createReturnTypeTemplate(Method method) {
        Type t = method.getGenericReturnType();
        Class<?> c = method.getReturnType();
        if (List.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c)) {
            return createTemplate(t);
        } else if (c.equals(void.class)) {
            return null;
        } else if (c.equals(Request.class)) {
            return null;
        } else {
            return createTemplate(c);
        }
    }
}
