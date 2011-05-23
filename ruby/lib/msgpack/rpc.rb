#
# MessagePack-RPC for Ruby
#
# Copyright (C) 2010-2011 FURUHASHI Sadayuki
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.
#
module MessagePack  #:nodoc:

# MessagePack-RPC is an inter-process messaging library that uses
# MessagePack[http://msgpack.sourceforge.net/] for object serialization.
# The goal of the project is providing fast and scalable messaging system
# for server, client and cluster applications.
#
# You can install MessagePack-RPC for Ruby using RubyGems.
#
#   gem install msgpack-rpc
#
#
# == Client API
#
# MessagePack::RPC::Client and MessagePack::RPC::SessionPool are for RPC clients.
#
#
# === Simple usage
#
# Client is subclass of Session. Use Session#call method to call remote methods.
#
#   require 'msgpack/rpc'
#   
#   client = MessagePack::RPC::Client.new('127.0.0.1', 18800)
#   
#   result = client.call(:methodName, arg1, arg2, arg3)
#
#   #     ---------- server
#   #    ^         |
#   #    |         |
#   # ---+         +----- client
#   #    call      join
#
#
# === Asynchronous call
#
# Use Session#call_async method to call remote methods asynchronously. It returns a Future. Use Future#get or Future#attach_callback to get actual result.
#
#   require 'msgpack/rpc'
#   
#   client = MessagePack::RPC::Client.new('127.0.0.1', 18800)
#   
#   # call two methods concurrently
#   future1 = client.call_async(:method1, arg1)
#   future2 = client.call_async(:method2, arg1)
#   
#   # join the results
#   result1 = future1.get
#   result2 = future2.get
#
#   #     ------------------ server
#   #    ^                 |
#   #    |        ---------|-------- server
#   #    |       ^         |       |
#   #    |       |         |       |
#   # ---+-------+-----    +-------+----- client
#   #    call    call      join    join
#
# === Asynchronous call with multiple servers
#
# Loop enables you to invoke multiple asynchronous calls for multiple servers concurrently.
# This is good for advanced network applications.
#
#   require 'msgpack/rpc'
#   
#   # create a event loop
#   loop = MessagePack::RPC::Loop.new
#
#   # connect to multiple servers
#   client1 = MessagePack::RPC::Client.new('127.0.0.1', 18801, loop)
#   client2 = MessagePack::RPC::Client.new('127.0.0.1', 18802, loop)
#   
#   # call two methods concurrently
#   future1 = client1.call_async(:method1, arg1)
#   future2 = client2.call_async(:method2, arg1)
#   
#   # join the results
#   result1 = future1.get
#   result2 = future2.get
#
#   #     ------------------ server-1  --- different servers
#   #    ^                 |             /
#   #    |        ---------|-------- server-2
#   #    |       ^         |       |
#   #    |       |         |       |
#   # ---+-------+-----    +-------+----- client
#   #    call    call      join    join
#
# === Connection pooling
#
# SessionPool#get_session returns a Session. It pools created session and enables you to reuse established connections.
#
#   require 'msgpack/rpc'
#   
#   sp = MessagePack::RPC::SessionPool.new
#   
#   client = sp.get_session('127.0.0.1', 18800)
#   
#   result = client.call(:methodName, arg1, arg2, arg3)
#
#
# == Server API
#
# MessagePack::RPC::Server is for RPC servers.
#
#
# === Simple usage
#
# The public methods of the handler class becomes callbale.
#
#   require 'msgpack/rpc'
#
#   class MyHandler
#     def methodName(arg1, arg2, arg3)
#       puts "received"
#       return "return result."
#     end
#   end
#
#   server = MessagePack::RPC::Server.new
#   server.listen('0.0.0.0', 18800, MyHandler.new)
#   server.run
#
#
# === Advance return
#
# You can use *yield* to send the result without returning.
#
#   class MyHandler
#     def method1(arg1)
#       yield("return result.")
#       puts "you can do something after returning the result"
#     end
#   end
#
#
# === Delayed return
#
# You can use AsyncResult to return results later.
#
#   class MyHandler
#     def method2(arg1)
#       as = MessagePack::RPC::AsyncResult.new
#       Thread.new do
#         sleep 10   # return result 10 seconds later.
#         as.result("return result.")
#       end
#       return as
#     end
#   end
#
#
# You can receive and send any objects that can be serialized by MessagePack.
# This means that the objects required to implement *to_msgpack(out = '')* method.
#
#
# == Transports
#
# You can use UDP and UNIX domain sockets instead of TCP.
#
#
# === For clients
#
# For clients, use MessagePack::RPC::UDPTransport or MessagePack::RPC::UNIXTransport.
#
#   require 'msgpack/rpc'
#   require 'msgpack/rpc/transport/udp'
#   
#   transport = MessagePack::RPC::UDPTransport.new
#   address = MessagePack::RPC::Address.new('127.0.0.1', 18800)
#   
#   client = MessagePack::RPC::Client.new(transport, address)
#   
#   result = client.call(:methodName, arg1, arg2, arg3)
#
# You can use transports for SessionPool.
#
#   require 'msgpack/rpc'
#   require 'msgpack/rpc/transport/udp'
#   
#   transport = MessagePack::RPC::UDPTransport.new
#   
#   sp = MessagePack::RPC::SessionPool.new(transport)
#   
#   client = sp.get_session('127.0.0.1', 18800)
#   
#   result = client.call(:methodName, arg1, arg2, arg3)
#
# === For servers
#
# For servers, use MessagePack::RPC::UDPServerTransport or MessagePack::RPC::UNIXServerTransport.
#
#   require 'msgpack/rpc'
#   require 'msgpack/rpc/transport/udp'
#   
#   class MyHandler
#     def methodName(arg1, arg2, arg3)
#       puts "received"
#       return "return result."
#     end
#   end
#   
#   address = MessagePack::RPC::Address.new('0.0.0.0', 18800)
#   listener = MessagePack::RPC::UDPServerTransport.new(address)
#
#   server = MessagePack::RPC::Server.new
#   server.listen(listener, MyHandler.new)
#   server.run
#
#
module RPC
end

end  # module MessagePack


require 'msgpack'
require 'socket'
require 'rev'
require 'msgpack/rpc/version'
require 'msgpack/rpc/address'
require 'msgpack/rpc/message'
require 'msgpack/rpc/exception'
require 'msgpack/rpc/loop'
require 'msgpack/rpc/future'
require 'msgpack/rpc/multi_future'
require 'msgpack/rpc/session'
require 'msgpack/rpc/session_pool'
require 'msgpack/rpc/dispatcher'
require 'msgpack/rpc/client'
require 'msgpack/rpc/server'
require 'msgpack/rpc/transport/base'
require 'msgpack/rpc/transport/tcp'

