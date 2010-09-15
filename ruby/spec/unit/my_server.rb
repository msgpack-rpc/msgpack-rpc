
	class MyServer
		@port = 65500
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

		def self.next_port
			@port += 1
		end


	def self.start_server
		port = MyServer.next_port

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



		private
		def hidden
		end



	end

