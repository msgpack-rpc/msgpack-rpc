package org.msgpack.rpc.util.codegen;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.Map;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtNewConstructor;
import javassist.CtNewMethod;
import javassist.Modifier;
import javassist.NotFoundException;

import org.msgpack.rpc.Dispatcher;
import org.msgpack.rpc.Request;

public class DynamicCodegenDispatcher implements Dispatcher {

    static class Constants {
        static final String POSTFIX_TYPE_NAME_INVOKER = "_$$_Invoker";

        static final String KEYWORD_MODIFIER_PUBLIC = "public";

        static final String KEYWORD_THROWS = "throws";

        static final String TYPE_NAME_VOID = void.class.getName();

        static final String TYPE_NAME_REQUEST = Request.class.getName();

        static final String TYPE_NAME_EXCEPTION = Exception.class.getName();

        static final String TYPE_NAME_INVOKER = Invoker.class.getName();

        static final String CHAR_NAME_SPACE = " ";

        static final String CHAR_NAME_DOT = ".";

        static final String CHAR_NAME_COMMA = ",";

        static final String CHAR_NAME_EQUAL = "=";

        static final String CHAR_NAME_UNDERSCORE = "_";

        static final String CHAR_NAME_SEMICOLON = ";";

        static final String CHAR_NAME_RIGHT_PARENTHESIS = ")";

        static final String CHAR_NAME_LEFT_PARENTHESIS = "(";

        static final String CHAR_NAME_RIGHT_CURLY_BRACHET = "}";

        static final String CHAR_NAME_LEFT_CURLY_BRACHET = "{";

        static final String METHOD_NAME_INVOKE = "invoke";

        static final String FIELD_NAME_TARGET = "_$$_target";

        static final String VARIABLE_NAME_TARGET = "_$$_t";

        static final String VARIABLE_NAME_REQUEST = "_$$_r";
    }

    public static class InvokersGenerator {
        private ClassPool pool;

        public InvokersGenerator() {
            pool = ClassPool.getDefault();
        }

        public Map<String, Invoker> generateInvokers(Object target) {
            Class<?> origClass = target.getClass();
            String origName = origClass.getName();
            CtClass origCtClass = null;
            try {
                origCtClass = pool.get(origName);
            } catch (NotFoundException e) {
                throw new DynamicCodegenException(e.getMessage(), e);
            }
            // TODO
            // add methods in superclass, and add only methods that has a
            // <code>Request</code> type as a 1st parameter type
            CtMethod[] methods = origCtClass.getDeclaredMethods();
            Map<String, Invoker> invokers = new HashMap<String, Invoker>();
            for (CtMethod method : methods) {
                try {
                    Invoker invoker = generateInvoker(target, origClass, method);
                    invokers.put(method.getName(), invoker);
                } catch (Exception e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            return invokers;
        }

        private Invoker generateInvoker(Object target, Class<?> origClass,
                CtMethod method) throws NotFoundException,
                CannotCompileException, Exception {
            String origName = origClass.getName();
            String methodName = method.getName();
            CtClass invokerCtClass = makeClass(origName, methodName);
            setInterface(invokerCtClass);
            addField(invokerCtClass, origName);
            addConstructor(invokerCtClass, origName);
            addMethod(invokerCtClass, methodName);
            Class<?> invokerClass = createClass(invokerCtClass);
            return newInvokerInstance(target, origClass, invokerClass);
        }

        private CtClass makeClass(String origName, String methodName) {
            StringBuilder sb = new StringBuilder();
            sb.append(origName).append(Constants.CHAR_NAME_UNDERSCORE).append(
                    methodName).append(Constants.POSTFIX_TYPE_NAME_INVOKER);
            String invokerName = sb.toString();
            CtClass invokerCtClass = pool.makeClass(invokerName);
            invokerCtClass.setModifiers(Modifier.PUBLIC);
            return invokerCtClass;
        }

        private void setInterface(CtClass invokerCtClass)
                throws NotFoundException {
            CtClass invokerInf = pool.get(Constants.TYPE_NAME_INVOKER);
            invokerCtClass.addInterface(invokerInf);
        }

        private void addField(CtClass invokerCtClass, String origName)
                throws CannotCompileException {
            // in this part, a created field is not initialized
            StringBuilder sb = new StringBuilder();
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC).append(
                    Constants.CHAR_NAME_SPACE).append(origName).append(
                    Constants.CHAR_NAME_SPACE).append(
                    Constants.FIELD_NAME_TARGET).append(
                    Constants.CHAR_NAME_SEMICOLON);
            System.out.println("invoker field: " + sb.toString());
            CtField targetCtField = CtField.make(sb.toString(), invokerCtClass);
            invokerCtClass.addField(targetCtField);
        }

        private void addConstructor(CtClass invokerCtClass, String origName)
                throws CannotCompileException {
            StringBuilder sb = new StringBuilder();
            // public Foo_m_$$_Invoker(Foo target) { _target = target; }
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC).append(
            // Constants.CHAR_NAME_SPACE).append(invokerCtClass.getName())
                    Constants.CHAR_NAME_SPACE).append(
                    invokerCtClass.getSimpleName()).append(
                    Constants.CHAR_NAME_LEFT_PARENTHESIS).append(origName)
                    .append(Constants.CHAR_NAME_SPACE).append(
                            Constants.VARIABLE_NAME_TARGET).append(
                            Constants.CHAR_NAME_RIGHT_PARENTHESIS).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.CHAR_NAME_LEFT_CURLY_BRACHET).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.FIELD_NAME_TARGET).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.CHAR_NAME_EQUAL).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.VARIABLE_NAME_TARGET).append(
                            Constants.CHAR_NAME_SEMICOLON).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.CHAR_NAME_RIGHT_CURLY_BRACHET);
            System.out.println("invoker constructor: " + sb.toString());
            CtConstructor newCtConstructor = CtNewConstructor.make(sb
                    .toString(), invokerCtClass);
            invokerCtClass.addConstructor(newCtConstructor);
        }

        private void addMethod(CtClass invokerCtClass, String methodName)
                throws CannotCompileException {
            StringBuilder sb = new StringBuilder();
            // public void invoke(Request _r) { _target.foo(_r); }
            sb.append(Constants.KEYWORD_MODIFIER_PUBLIC).append(
                    Constants.CHAR_NAME_SPACE).append(Constants.TYPE_NAME_VOID)
                    .append(Constants.CHAR_NAME_SPACE).append(
                            Constants.METHOD_NAME_INVOKE).append(
                            Constants.CHAR_NAME_LEFT_PARENTHESIS).append(
                            Constants.TYPE_NAME_REQUEST).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.VARIABLE_NAME_REQUEST).append(
                            Constants.CHAR_NAME_RIGHT_PARENTHESIS).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.KEYWORD_THROWS).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.TYPE_NAME_EXCEPTION).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.CHAR_NAME_LEFT_CURLY_BRACHET).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.FIELD_NAME_TARGET).append(
                            Constants.CHAR_NAME_DOT).append(methodName).append(
                            Constants.CHAR_NAME_LEFT_PARENTHESIS).append(
                            Constants.VARIABLE_NAME_REQUEST).append(
                            Constants.CHAR_NAME_RIGHT_PARENTHESIS).append(
                            Constants.CHAR_NAME_SEMICOLON).append(
                            Constants.CHAR_NAME_SPACE).append(
                            Constants.CHAR_NAME_RIGHT_CURLY_BRACHET);
            System.out.println("invoker method: " + sb.toString());
            CtMethod newCtMethod = CtNewMethod.make(sb.toString(),
                    invokerCtClass);
            invokerCtClass.addMethod(newCtMethod);
        }

        private Class<?> createClass(CtClass invokerCtClass)
                throws CannotCompileException {
            return invokerCtClass.toClass(null, null);
        }

        private Invoker newInvokerInstance(Object target, Class<?> origClass,
                Class<?> invokerClass) throws Exception {
            Constructor<?> cons = invokerClass
                    .getConstructor(new Class[] { origClass });
            return (Invoker) cons.newInstance(target);
        }
    }

    public interface Invoker {
        void invoke(Request reqest) throws Exception;
    }

    private Map<String, Invoker> invokers;

    public DynamicCodegenDispatcher(Object target) {
        InvokersGenerator gen = new InvokersGenerator();
        invokers = gen.generateInvokers(target);
    }

    @Override
    public void dispatch(Request request) throws Exception {
        Invoker invoker = invokers.get(request.getMethodName());
        if (invoker == null) {
            // FIXME
            throw new IOException(".CallError.NoMethodError");
        }
        invoker.invoke(request);
    }

    public static void main(String[] args) throws Exception {
        Class c = Invoker.class;
        System.out.println("class name1: " + c.getName());
        System.out.println("class name2: " + c.getCanonicalName());
        System.out.println("class name3: " + c.getSimpleName());

        CtClass cc = ClassPool.getDefault().get(Invoker.class.getName());
        System.out.println("ct class name1: " + cc.getName());
        System.out.println("ct class name2: " + cc.getSimpleName());
        System.out
                .println("ct class name3: " + cc.getComponentType().getName());
    }
}