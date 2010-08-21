//
// MessagePack-RPC for Java
//
// Copyright (C) 2010 Kazuki Ohta
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
package org.msgpack.rpc.server;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.channels.Channel;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.msgpack.rpc.Constants;

public class RPCServerHandler extends SimpleChannelHandler {
    protected final Object handler;
    protected Map<String, Method> methodMap;

    public RPCServerHandler(Object handler) {
        super();
        this.handler = handler;
        
        // 2010/06/29 Kazuki Ohta <kazuki.ohta@gmail.com>
        // Java supports function overloading, but it's not happy for RPC
        // systems which distinguish the method by the name. We should throw
        // the exception when we found the overloading here?
        Method[] handlerMethods = handler.getClass().getMethods();
        Map<String, Method> m = new HashMap<String, Method>();
        for (Method method : handlerMethods)
            m.put(method.getName(), method);
        this.methodMap = Collections.unmodifiableMap(m);
    }
    
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent ev) {
        ev.getCause().printStackTrace();
        Channel ch = (Channel) ev.getChannel();
        try {
            ch.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void messageReceived(ChannelHandlerContext ctx, MessageEvent e) throws Exception {
        Object obj = e.getMessage();
        if (obj == null) return;
        if (!(obj instanceof AbstractList<?>)) return;
        List<Object> list = (List<Object>)obj;
        for (Object o: list)
            processOneMessage(e, o);
    }
    
    protected void processOneMessage(MessageEvent e, Object o) throws Exception {
        AbstractList<?> a = (AbstractList<?>)o;
        if (a.size() != 4)
            throw new IOException("Invalid MPRPC"); // TODO

        Object type   = a.get(0);
        Object msgid  = a.get(1);
        Object method = a.get(2);
        Object params = a.get(3);
        if (((Number)type).intValue() != Constants.TYPE_REQUEST)
            throw new IOException("Invalid MPRPC"); // TODO
        if (!(method instanceof byte[]))
            throw new IOException("Invalid method"); // TODO

        Object handlerResult = null;
        String errorMessage = null;
        try {
            AbstractList<?> paramList;
            if (params instanceof AbstractList<?>) {
                paramList = (AbstractList<?>)params;
            } else {
                paramList = new ArrayList<Object>();
            }
            handlerResult = callMethod(new String((byte[])method), paramList);
        } catch (Exception rpc_e) {
            errorMessage = rpc_e.getMessage();
        }

        ArrayList<Object> response = new ArrayList<Object>();
        response.add(Constants.TYPE_RESPONSE);
        response.add(msgid);
        response.add(errorMessage);
        response.add(handlerResult);

        e.getChannel().write(response, e.getRemoteAddress());
    }

    protected Object callMethod(String method, AbstractList<?> params) throws Exception {
        int nParams = params.size();
        Method m = methodMap.get(method);
        if (m == null || nParams != m.getParameterTypes().length)
            throw new IOException("No such method");
        return m.invoke(handler, params.toArray());
    }
}
