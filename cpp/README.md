MessagePack-RPC for C++
=======================


## Requirements

Following programs are requred to build:

  - gcc >= 4.1 with C++ support
  - [MessagePack for C++](http://msgpack.org/) >= 0.5.2
  - [mpio](http://github.com/frsyuki/mpio) >= 0.3.5


## Installation

Configure and install in the usual way:

    $ ./bootstrap  # if needed
    $ ./configure
    $ make
    $ sudo make install


## Usage

[Test cases](http://github.com/msgpack/msgpack-rpc/tree/master/cpp/test/) will give you a sample usage.

  - [echo server](http://github.com/msgpack/msgpack-rpc/blob/master/cpp/test/echo_server.h)
  - [synchronous method call](http://github.com/msgpack/msgpack-rpc/blob/master/cpp/test/sync_call.cc)
  - [asynchronous method call](http://github.com/msgpack/msgpack-rpc/blob/master/cpp/test/async_call.cc)
  - [callback-style method call](http://github.com/msgpack/msgpack-rpc/blob/master/cpp/test/callback.cc)
  - [notify](http://github.com/msgpack/msgpack-rpc/blob/master/cpp/test/notify.cc)


## Example

### Simple client

    #include <msgpack/rpc/client.h>
    #include <iostream>
    
    int main(void)
    {
    	msgpack::rpc::client c("127.0.0.1", 9090);
    	int result = c.call("add", 1, 2).get<int>();
    	std::cout << result << std::endl;
    }


### Simple server

    #include <msgpack/rpc/server.h>
    
    class myserver : public msgpack::rpc::server::base {
    public:
    	void add(msgpack::rpc::request req, int a1, int a2)
    	{
    		req.result(a1 + a2);
    	}
    
    public:
    	void dispatch(msgpack::rpc::request req)
    	try {
    		std::string method;
    		req.method().convert(&method);
    
    		if(method == "add") {
    			msgpack::type::tuple<int, int> params;
    			req.params().convert(&params);
    			add(req, params.get<0>(), params.get<1>());
    
    		} else {
    			req.error(msgpack::rpc::NO_METHOD_ERROR);
    		}
    
    	} catch (msgpack::type_error& e) {
    		req.error(msgpack::rpc::ARGUMENT_ERROR);
    		return;
    
    	} catch (std::exception& e) {
    		req.error(std::string(e.what()));
    		return;
    	}
    };
    
    int main(void)
    {
    	myserver svr;
    	svr.instance.listen("0.0.0.0", 9090);
    	svr.instance.run(4);  // run 4 threads
    }


IDL parser and code generator is under development. It will resolve the problem that users have to implement verbose dispatch() function.


## License

    Copyright (C) 2008-2010 FURUHASHI Sadayuki <frsyuki _at_ users.sourceforge.jp>
    
       Licensed under the Apache License, Version 2.0 (the "License");
       you may not use this file except in compliance with the License.
       You may obtain a copy of the License at
    
           http://www.apache.org/licenses/LICENSE-2.0
    
       Unless required by applicable law or agreed to in writing, software
       distributed under the License is distributed on an "AS IS" BASIS,
       WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       See the License for the specific language governing permissions and
       limitations under the License.

