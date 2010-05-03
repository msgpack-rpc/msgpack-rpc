package org.msgpack.rpc.client.netty;

import java.net.ConnectException;

import org.jboss.netty.channel.ChannelEvent;
import org.jboss.netty.channel.ChannelPipelineCoverage;
import org.jboss.netty.channel.ChannelStateEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.msgpack.rpc.client.TCPSocket;

@ChannelPipelineCoverage("all")
public class RPCClientHandler extends SimpleChannelHandler {
	protected TCPSocket sock;
	
	public RPCClientHandler(TCPSocket sock) {
    	super();
    	this.sock = sock;
    }

    @Override
    public void channelConnected(ChannelHandlerContext ctx, ChannelStateEvent ev) {
    	try {
    		sock.onConnected();
    	} catch (Exception e) {
    		e.printStackTrace();
    		sock.onConnectFailed();
		}
    }

    @Override
    public void messageReceived(ChannelHandlerContext ctx, MessageEvent ev) {
        try {
        	sock.onMessageReceived(ev.getMessage());
        } catch (Exception e) {
        	e.printStackTrace();
        	sock.onFailed();
        }
	}

    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent ev) {
    	Throwable e = ev.getCause();
    	if (e instanceof ConnectException)
    		sock.onConnectFailed();
    	else
    		sock.onFailed();
    }
}
