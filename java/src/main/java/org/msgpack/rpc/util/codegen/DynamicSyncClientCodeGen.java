package org.msgpack.rpc.util.codegen;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;

import org.msgpack.MessagePackObject;
import org.msgpack.util.codegen.DynamicCodeGenException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class DynamicSyncClientCodeGen extends DynamicClientCodeGenBase {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicSyncClientCodeGen.class);

    @Override
    protected CtClass makeClass(String handlerName) throws NotFoundException {
        StringBuilder sb = new StringBuilder();
        sb.append(handlerName);
        sb.append(POSTFIX_TYPE_NAME_SYNCCLIENT);
        sb.append(inc());
        String invokerName = sb.toString();
        CtClass newCtClass = pool.makeClass(invokerName);
        newCtClass.setModifiers(Modifier.PUBLIC);
        return newCtClass;
    }

    @Override
    protected void addHandlerMethod(CtClass newCtClass, Method method, int index) {
        StringBuilder sb = new StringBuilder();
        insertHandlerMethodBody(sb, method, index);
        try {
            LOG.trace("handler method body src: " + sb.toString());
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
        } catch (Exception e) {
            DynamicCodeGenException ex = new DynamicCodeGenException(e
                    .getMessage()
                    + ": " + sb.toString(), e);
            LOG.error(ex.getMessage(), ex);
            throw ex;
        }
    }

    private void insertHandlerMethodBody(StringBuilder sb, Method method, int i) {
        sb.append(CHAR_NAME_LEFT_CURLY_BRACKET);
        sb.append(CHAR_NAME_SPACE);
        Class<?> c = method.getReturnType();
        if (!c.equals(void.class)) {
            // MessagePackObject _$$_mpo = _$$_client.callApply("m0", $args);
            Object[] args0 = new Object[] { MessagePackObject.class.getName(),
                    VARIABLE_NAME_MPO, VARIABLE_NAME_CLIENT, method.getName() };
            sb.append(String.format(STATEMENT_CLIENT_CALLAPPLY_01, args0));
            // return ($r)_$$_templates[i].convert(_$$_mpo);
            Object[] args1 = new Object[] { VARIABLE_NAME_TEMPLATES, i,
                    VARIABLE_NAME_MPO };
            sb.append(String.format(STATEMENT_CLIENT_CONVERT_01, args1));
        } else {
            // _$$_client.callApply("m0", $args);
            Object[] args0 = new Object[] { VARIABLE_NAME_CLIENT,
                    method.getName() };
            sb.append(String.format(STATEMENT_CLIENT_CALLAPPLY_02, args0));
        }
        sb.append(CHAR_NAME_RIGHT_CURLY_BRACKET);
    }
}
