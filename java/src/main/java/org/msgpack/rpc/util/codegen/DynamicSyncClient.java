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

import java.net.InetSocketAddress;
import java.net.UnknownHostException;

import org.msgpack.rpc.Client;
import org.msgpack.rpc.EventLoop;
import org.msgpack.rpc.transport.ClientTransport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class DynamicSyncClient {
    private static Logger LOG = LoggerFactory
            .getLogger(DynamicSyncClient.class);

    private static DynamicSyncClientCodeGen gen;

    public static <T> T create(String host, int port, Class<T> handlerType)
            throws UnknownHostException {
        return create(new Client(host, port), handlerType);
    }

    public static <T> T create(ClientTransport transport,
            InetSocketAddress address, Class<T> handlerType) {
        return create(new Client(transport, address), handlerType);
    }

    public static <T> T create(ClientTransport transport,
            InetSocketAddress address, EventLoop loop, Class<T> handlerType) {
        return create(new Client(transport, address, loop), handlerType);
    }

    @SuppressWarnings("unchecked")
    public static <T> T create(Client client, Class<T> handlerType) {
        LOG.info("create an instance of " + DynamicSyncClient.class.getName()
                + ": handler type: " + handlerType.getName());
        if (gen == null) {
            gen = new DynamicSyncClientCodeGen();
        }

        String handlerName = handlerType.getName();
        Class<T> clientClass = (Class<T>) gen.getCache(handlerName);
        if (clientClass == null) {
            clientClass = (Class<T>) gen.generateClientClass(handlerName,
                    handlerType);
            gen.setCache(handlerName, clientClass);
        }
        return (T) gen.newClientInstance(clientClass, client, handlerName);
    }
}
