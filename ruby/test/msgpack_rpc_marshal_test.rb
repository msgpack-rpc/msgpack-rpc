#!/usr/bin/env ruby

require File.expand_path(File.dirname(__FILE__)) + '/test_helper.rb'
require File.expand_path(File.dirname(__FILE__)) + '/msgpack_rpc_test.rb'


p
$port = 65500

class MessagePackRPCMarshalTest < Test::Unit::TestCase
  include MessagePackRPCTestBase

  def setup
    @server_cls = MessagePack::RPC::Marshal::Server
    @client_cls = MessagePack::RPC::Marshal::Client
  end

	def test_pool
    # Overrides the default pool test to use
    # RPC::Marshal::SessionPool instead of the normal RPC::SessionPool
		svr, cli = start_server

		sp = MessagePack::RPC::Marshal::SessionPool.new
		s = sp.get_session('127.0.0.1', cli.port)

		result = s.call(:hello)
		assert_equal(result, "ok")

		result = s.call(:sum, 1, 2)
		assert_equal(result, 3)

		sp.close
		cli.close
		svr.stop
	end

	def test_echo_marshal
		svr, cli = start_server

		result = cli.call(:echo_marshal, MarshalDemo.new("ok"))
		assert_equal(result.value, "ok")

		cli.close
		svr.stop
	end

	def test_modify_marshal
		svr, cli = start_server

		result = cli.call(:modify_marshal, MarshalDemo.new("fail"))
		assert_equal(result.value, "ok")

		cli.close
		svr.stop
	end

	def test_remote_marshal
		svr, cli = start_server

		result = cli.call(:remote_marshal)
		assert_equal(result.value, "ok")

		cli.close
		svr.stop
	end
end

