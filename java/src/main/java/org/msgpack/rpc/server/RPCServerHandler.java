package org.msgpack.rpc.server;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.channels.Channel;
import java.util.AbstractList;
import java.util.ArrayList;

import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.msgpack.rpc.Constants;

@ChannelPipelineCoverage("all")
public class RPCServerHandler extends SimpleChannelHandler {
    protected Object handler = null;
    protected Method[] handlerMethods = null;

    public RPCServerHandler(Object handler) {
        super();
        this.handler = handler;
        this.handlerMethods = handler.getClass().getMethods();
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
        AbstractList<?> a = (AbstractList<?>)e.getMessage();
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
            handlerResult = callMethod(handler, new String((byte[])method), paramList);
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

    protected Object callMethod(Object handler, String method, AbstractList<?> params) throws Exception {
        Method m = findMethod(handler, method, params);
        if (m == null) throw new IOException("No such method");
        return m.invoke(handler, params.toArray());
    }

    protected Method findMethod(Object handler, String method, AbstractList<?> params) {
        int nParams = params.size();
        Method[] ms = handlerMethods;
        for (int i = 0; i < ms.length; i++) {
            Method m = ms[i];
            if (!method.equals(m.getName())) continue;
            if (nParams != m.getParameterTypes().length) continue;
            return m;
        }
        return null;
    }
}
