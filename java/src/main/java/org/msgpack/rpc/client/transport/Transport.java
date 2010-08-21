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
package org.msgpack.rpc.client.transport;

import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.client.Session;

abstract public class Transport {
    protected final Session session;
    protected final EventLoop loop;
    
    public Transport(Session session, EventLoop loop) {
        this.session = session;
        this.loop = loop;
    }

    abstract public void sendMessage(Object msg) throws Exception;
    abstract public void tryClose();
}
