#!/usr/bin/env ruby
require File.dirname(__FILE__) + '/test_helper.rb'
require 'test/unit'

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


	def test_listen
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		svr.close
	end


	def test_call
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		result = cli.call(:hello)
		assert_equal(result, "ok")

		result = cli.call(:sum, 1, 2)
		assert_equal(result, 3)

		cli.close
	end


	def test_send
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		req1 = cli.send(:hello)
		req2 = cli.send(:sum, 1, 2)

		req1.join
		req2.join

		assert_equal(req1.result, "ok")
		assert_nil(req1.error)
		assert_equal(req2.result, 3)
		assert_nil(req2.error)

		cli.close
	end


	def test_callback
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		count = 0

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

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
	end


	def test_hidden
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		count = 0

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		rejected = false
		begin
			cli.call(:hidden)
		rescue MessagePack::RPC::RemoteError
			rejected = true
		end

		assert_equal(rejected, true)

		cli.close
	end


	def test_exception
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		raised = false
		begin
			cli.call(:exception)
		rescue MessagePack::RPC::RemoteError
			assert_equal($!.message, "raised")
			raised = true
		end

		assert_equal(raised, true)

		cli.close
	end


	def test_async
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		result = cli.call(:async)
		assert_equal(result, "async")

		cli.close
	end


	def test_async_exception
		port = $port += 1

		svr = MessagePack::RPC::Server.new
		svr.listen("0.0.0.0", port, MyServer.new(svr))
		Thread.start do
			svr.run
		end

		cli = MessagePack::RPC::Client.new("127.0.0.1", port)
		cli.timeout = 10

		raised = false
		begin
			cli.call(:async_exception)
		rescue MessagePack::RPC::RemoteError
			assert_equal($!.message, "async")
			raised = true
		end

		assert_equal(raised, true)

		cli.close
	end


	def test_loop
		port = $port += 1

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
		port = $port += 1

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

