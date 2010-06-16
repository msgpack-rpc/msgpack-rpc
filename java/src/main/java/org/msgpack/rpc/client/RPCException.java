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

import java.io.IOException;

public class RPCException extends IOException {
    private static final long serialVersionUID = 1L;

    /**                                                                                                                                                                              
     * Constructs exception with the specified detail message.                                                                                                                       
     *                                                                                                                                                                               
     * @param messages detailed message.                                                                                                                                             
     */
    public RPCException(final String message) {
      super(message);
    }

    /**                                                                                                                                                                              
     * Constructs exception with the specified detail message and cause.                                                                                                             
     *                                                                                                                                                                               
     * @param message message.                                                                                                                                                       
     * @param cause that cause this exception                                                                                                                                        
     * @param cause the cause (can be retried by the {@link #getCause()} method).                                                                                                    
     *          (A <tt>null</tt> value is permitted, and indicates that the cause                                                                                                    
     *          is nonexistent or unknown.)                                                                                                                                          
     */
    RPCException(final String message, final Throwable cause) {
      super(message, cause);
    }
}
