#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/test_helper.rb'

$port = 65500

class MessagePackRPCTest < Test::Unit::TestCase

	class MyServer
		def initialize(svr)
			@svr = svr
		end

		def hello
			"ok"
		end

		def sum(a, b)
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


	def next_port
		port = $port += 1
	end


	def test_listen
		port = next_port

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		svr.close
	end


	def start_server
		port = next_port

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
			svr.close
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		return svr, cli
	end


	def test_call
		svr, cli = start_server

		result = cli.call(:hello)
		assert_equal(result, "ok")

		result = cli.call(:sum, 1, 2)
		assert_equal(result, 3)

		cli.close
		svr.stop
	end


	def test_send
		svr, cli = start_server

		req1 = cli.send(:hello)
		req2 = cli.send(:sum, 1, 2)

		req1.join
		req2.join

		assert_equal(req1.result, "ok")
		assert_nil(req1.error)
		assert_equal(req2.result, 3)
		assert_nil(req2.error)

		cli.close
		svr.stop
	end


	def test_callback
		svr, cli = start_server

		count = 0

		cli.callback(:hello) do |error, result|
			assert_equal(result, "ok")
			assert_nil(error)
			count += 1
		end

		cli.callback(:sum, 1, 2) do |error, result|
			assert_equal(result, 3)
			assert_nil(error)
			count += 1
		end

		while count < 2
			cli.loop.run_once
		end

		cli.close
		svr.stop
	end


	def test_notify
		svr, cli = start_server

		cli.notify(:hello)
		cli.notify(:sum, 1, 2)

		cli.close
	end


	def test_hidden
		svr, cli = start_server

		count = 0

		rejected = false
		begin
			cli.call(:hidden)
		rescue MessagePack::RPC::RemoteError
			rejected = true
		end

		assert_equal(rejected, true)

		cli.close
		svr.stop
	end


	def test_exception
		svr, cli = start_server

		raised = false
		begin
			cli.call(:exception)
		rescue MessagePack::RPC::RemoteError
			assert_equal($!.message, "raised")
			raised = true
		end

		assert_equal(raised, true)

		cli.close
		svr.stop
	end


	def test_async
		svr, cli = start_server

		result = cli.call(:async)
		assert_equal(result, "async")

		cli.close
		svr.stop
	end


	def test_async_exception
		svr, cli = start_server

		raised = false
		begin
			cli.call(:async_exception)
		rescue MessagePack::RPC::RemoteError
			assert_equal($!.message, "async")
			raised = true
		end

		assert_equal(raised, true)

		cli.close
		svr.stop
	end


	def test_pool
		svr, cli = start_server

		sp = MessagePack::RPC::SessionPool.new
		s = sp.get_session('127.0.0.1', cli.port)

		result = s.call(:hello)
		assert_equal(result, "ok")

		result = s.call(:sum, 1, 2)
		assert_equal(result, 3)

		sp.close
		cli.close
		svr.stop
	end


	def test_loop
		port = next_port

		loop = MessagePack::RPC::Loop.new

		svr = MessagePack::RPC::Server.new(loop)
		svr.listen("0.0.0.0", port, MyServer.new(svr))

		cli = MessagePack::RPC::Client.new("127.0.0.1", port, loop)
		cli.timeout = 10

		count = 0

		cli.callback(:hello) do |error, result|
			assert_equal(result, "ok")
			assert_nil(error)
			count += 1
		end

		cli.callback(:sum, 1, 2) do |error, result|
			assert_equal(result, 3)
			assert_nil(error)
			count += 1
		end

		while count < 2
			loop.run_once
		end

		cli.close
		svr.close
	end


	def test_timeout
		port = next_port

		lsock = TCPServer.new("0.0.0.0", port)

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 1

		timeout = false
		begin
			cli.call(:hello)
		rescue MessagePack::RPC::TimeoutError
			timeout = true
		end

		assert_equal(timeout, true)

		cli.close
		lsock.close
	end
end

