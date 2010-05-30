#!/usr/bin/env ruby

#require File.dirname(__FILE__) + '/test_helper.rb'
# require 'test/unit'

# $LOAD_PATH.unshift File.dirname(__FILE__)+'/../ruby/lib'
# $LOAD_PATH.unshift "../ruby"
# $LOAD_PATH.unshift '../ruby/lib/msgpack'
# p $LOAD_PATH

require 'msgpack/rpc'

$port = 65500
port = $port

class MyServer
  def initialize(svr)
    @svr = svr
  end
  
  def hello
    p "hello"
    "ok"
  end
  
  def sum(a, b)
    p "hello, sum!!!"
    a + b
  end
  
  def exception
    raise "raised"
  end
  
  def async
    as = MessagePack::RPC::AsyncResult.new
    @svr.start_timer(1, false) do
      as.result "async"
    end
    as
  end
  
  def async_exception
    as = MessagePack::RPC::AsyncResult.new
    @svr.start_timer(1, false) do
      as.error "async"
    end
    as
  end
  
  private
  def hidden
  end
end

svr = MessagePack::RPC::Server.new
svr.listen("0.0.0.0", port, MyServer.new(svr))

svr.close


def start_server port
  
  svr = MessagePack::RPC::Server.new
  svr.listen("0.0.0.0", port, MyServer.new(svr))
  Thread.start do
    svr.run
#    svr.close
  end
  
  cli = MessagePack::RPC::Client.new("127.0.0.1", port)
  cli.timeout = 10
  
  return svr, cli
end

#  result = cli.call(:hello)
#  assert_equal(result, "ok")
  
#  result = cli.call(:sum, 1, 2)
#  assert_equal(result, 3)
  
svr, cli = start_server port
print "listening to port #{port}.\n"
gets

cli.close
svr.stop
