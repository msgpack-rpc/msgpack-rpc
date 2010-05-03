package org.msgpack.rpc;

import java.io.IOException;

import org.msgpack.rpc.client.Client;
import org.msgpack.rpc.client.EventLoop;
import org.msgpack.rpc.server.*;

/**
 * Hello world!
 *
 */

public class App 
{
    public int hello0() {
    	System.out.println("hello0");
        return 0;
    }
    public int hello1(int a) {
    	System.out.println("hello1");
        return 1;
    }
    public int hello2(int a, int b) {
    	System.out.println("hello2");
        return 2;
    }
    
    /*

    */
}
