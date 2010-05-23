package org.msgpack.rpc.client.transport;

import static org.jboss.netty.channel.Channels.pipeline;

import java.io.IOException;
import java.net.InetSocketAddress;

import org.jboss.netty.bootstrap.ConnectionlessBootstrap;
import org.jboss.netty.channel.ChannelHandlerContext;
import org.jboss.netty.channel.ChannelPipeline;
import org.jboss.netty.channel.ChannelPipelineFactory;
import org.jboss.netty.channel.ExceptionEvent;
import org.jboss.netty.channel.MessageEvent;
import org.jboss.netty.channel.SimpleChannelHandler;
import org.jboss.netty.channel.socket.DatagramChannel;
import org.msgpack.rpc.client.Address;
import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.netty.RPCRequestEncoder;
import org.msgpack.rpc.client.netty.RPCResponseDecoder;

class DatagramClientHandler extends SimpleChannelHandler {
    protected UDPSocket sock;
    
    public DatagramClientHandler(UDPSocket sock) {
        super();
        this.sock = sock;
    }
    
    @Override
    public void messageReceived(ChannelHandlerContext ctx, MessageEvent ev) {
        try {
            sock.onMessageReceived(ev.getMessage());
        } catch (Exception e) {
            e.printStackTrace();
            sock.onFailed(e);
        }
    }
      
    @Override
    public void exceptionCaught(ChannelHandlerContext ctx, ExceptionEvent ev) {
        Throwable e = ev.getCause();
        sock.onFailed(new IOException(e.getMessage()));
    }
}

class UDPClientPipelineFactory implements ChannelPipelineFactory {
    protected UDPSocket sock;
    
    public UDPClientPipelineFactory(UDPSocket sock) {
        this.sock = sock;
    }

    public ChannelPipeline getPipeline() throws Exception {
        ChannelPipeline pipeline = pipeline();
        pipeline.addLast("encoder", new RPCRequestEncoder());        
        pipeline.addLast("decoder", new RPCResponseDecoder());
        pipeline.addLast("client", new DatagramClientHandler(sock));
        return pipeline;
    }
}

public class UDPSocket {
    protected final Address address;
    protected final EventLoop loop;
    protected final UDPTransport transport;
    
    // netty-specific
    protected ConnectionlessBootstrap bootstrap;
    protected DatagramChannel channel;

    public UDPSocket(Address address, EventLoop loop, UDPTransport transport) {
        this.address = address;
        this.loop = loop;
        this.transport = transport;
        this.bootstrap = loop.createDatagramBootstrap();
        bootstrap.setOption("broadcast", "false");
        bootstrap.setOption("sendBufferSize", 65536);
        bootstrap.setOption("receiveBufferSize", 65536);
        bootstrap.setPipelineFactory(new UDPClientPipelineFactory(this));
        this.channel = null;
    }
   
    public synchronized void trySend(Object msg) throws Exception {
        if (channel == null)
            channel = (DatagramChannel)bootstrap.bind(new InetSocketAddress(0));
        channel.write(msg, new InetSocketAddress(address.getHost(), address.getPort()));
    }
    
    public synchronized void tryClose() {
        if (channel != null)
            channel.close().awaitUninterruptibly();
        channel = null;
    }

    public void onMessageReceived(Object replyObject) throws Exception {
        transport.onMessageReceived(replyObject);
    }
    
    public void onFailed(Exception e) {
        transport.onFailed(e);
    }
}
