package org.msgpack.inrealworld;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.msgpack.rpc.Client;
import org.msgpack.rpc.loop.EventLoop;

import java.io.*;
import java.net.UnknownHostException;

/**
 *
 * Repeat kill server process and restart.
 *
 * You must run this test using "mvn test" or current directory = ${project.toot}
 *
 * User: takeshita
 * Create: 11/10/11 16:01
 */
public class KillProcessTest {

    String classPath = null;
    File serverRoot = null;

    @Before
    public void init(){

        String osName = System.getProperty("os.name");

        // gather dependencies from maven repository
        ProcessBuilder builder;
        //check os
        if(osName.startsWith("Windows")){
            builder = new ProcessBuilder("mvn.bat","dependency:copy-dependencies");
        }else{
            builder = new ProcessBuilder("mvn","dependency:copy-dependencies");
        }
        try {
            Process p = builder.start();
        }  catch (IOException e) {
            e.printStackTrace();
            Assert.fail();
        }
        File classPath = new File("target/dependency");
        File classRoot = new File("target/classes");

        // find jar files and build classpath
        StringBuilder sb = new StringBuilder();
        sb.append(".");
        for(File f : classPath.listFiles()){
            String abs = f.getAbsolutePath();
            if(abs.endsWith(".jar")){
                sb.append(";" + abs);
            }
        }

        this.classPath = sb.toString();
        this.serverRoot = classRoot;

        System.out.println("CP:" + this.classPath + " SR:" + serverRoot);

    }


    /**
     * Run test server on wnother process
     * @param mainClass
     * @return
     */
    public Process runServer(Class<?> mainClass){

        ProcessBuilder builder = new ProcessBuilder("java","-classpath",classPath, mainClass.getName());
        builder.directory(serverRoot);
        builder.redirectErrorStream(true);
        Process p = null;
        try {
            p = builder.start();
            return p;
        } catch (IOException e) {
            e.printStackTrace();
            Assert.fail();
            return null;
        }
    }



    String readAsString(InputStream is) throws IOException {
        byte[] data = new byte[1000000];// about 1M
		try {
            int read = is.read(data);
            return new String(data,0,read);
		} finally {
            is.close();
		}
    }


    @Test
    public void testConnectAndDisconnect() {


        Process p = null;//runServer(KillProcessServer.class);


        try{
            System.out.println("start");
		    EventLoop loop = EventLoop.start();
            Client client = new Client("localhost", KillProcessServer.PORT,loop);
            client.setRequestTimeout(1000);
            KillProcessInterface proxy =  client.proxy(KillProcessInterface.class);


            p = runServer(KillProcessServer.class);
            Thread.sleep(1000);

            System.out.println("Begin access");
            System.out.println(proxy.ping());

            System.out.println(proxy.clearFile());
            System.out.println( proxy.sendMessage("1"));
            proxy.sendMessage("2");

            System.out.println("Success first access");

            for(int i = 0;i < 5;i++){
                p.destroy();
                Thread.sleep(100);
                p = runServer(KillProcessServer.class);
                proxy.sendMessage((i + 3) + "");
            }

            System.out.println("Success second access");

            String f = readAsString(new FileInputStream("target/classes/" + KillProcessServer.LOG_FILE));
            System.out.println("File:" + f);
            Assert.assertEquals("1234567",f);

        } catch (FileNotFoundException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            Assert.fail();
        } catch (UnknownHostException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            Assert.fail();
        } catch (IOException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            Assert.fail();
        } catch (InterruptedException e) {
            e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
            Assert.fail();
        } finally{
            if(p != null){
                p.destroy();
            }
        }

    }
}
