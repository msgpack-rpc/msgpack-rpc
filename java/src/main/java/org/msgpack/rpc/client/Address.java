package org.msgpack.rpc.client;

/**
 * The class to represent the network address.
 * Currently, only IPv4 is supported in this version.
 *
 * @TODO support IPv6
 * @TODO consider about UNIX domain soket.
 * But Java doesn't support that.
 */
public class Address {
    protected String host;
    protected int port;
    
    Address(String host, int port) {
        this.host = host;
        this.port = port;
    }
    
    /**
     * Get the hostname.
     * @return the hostname.
     */
    public String getHost() {
        return host;
    }
    
    /**
     * Get the port number.
     * @return the port number.
     */
    public int getPort() {
        return port;
    }
}
