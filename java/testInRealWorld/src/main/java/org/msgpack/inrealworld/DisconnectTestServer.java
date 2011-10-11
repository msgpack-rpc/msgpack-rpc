package org.msgpack.inrealworld;

import org.msgpack.rpc.Server;
import org.msgpack.rpc.loop.EventLoop;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * User: takeshita
 * Create: 11/10/11 16:00
 */
public class DisconnectTestServer implements  DisconnectTestInterface{

    public static final int PORT = 14522;
    public static final String LOG_FILE = "disconnect.log";

    public static void main(String[] args){

        String safeFilename = LOG_FILE;
        int port = PORT;


		EventLoop loop = EventLoop.start();
		Server svr = new Server(loop);

        try{
            System.out.println("waiting port " + port);
            DisconnectTestServer server =new DisconnectTestServer(safeFilename);
            svr.serve(server);
            svr.listen(port);
            System.out.println("start");


        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    File file;

    public DisconnectTestServer(String saveFile){
        file = new File(saveFile);

    }


    public boolean sendMessage(String message) {

        FileWriter writer = null;
        try{
            writer= new FileWriter(file,true);
            writer.write(message);
        }catch(IOException e){
            e.printStackTrace();
        }finally {
            if(writer != null){
                try {
                    writer.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return true;

    }

    public boolean clearFile() {
        if(file.exists()){
            file.delete();
        }
        System.out.println("Delete log file");
        return true;
    }

    public String ping(){
        return "OK";
    }

}
