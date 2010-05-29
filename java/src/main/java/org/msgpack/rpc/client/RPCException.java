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
