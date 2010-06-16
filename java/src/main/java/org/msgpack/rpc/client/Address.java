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
