package org.msgpack.inrealworld;

/**
 * User: takeshita
 * Create: 11/10/11 16:02
 */
public interface KillProcessInterface {


    public boolean sendMessage( String message);
    public boolean clearFile();
    public String ping();

}
