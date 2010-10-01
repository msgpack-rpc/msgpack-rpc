require File.join(File.expand_path(File.dirname(__FILE__)), 'spec_helper.rb')
require File.join(File.expand_path(File.dirname(__FILE__)), 'my_server.rb')
include MyServerTest

describe 'MessagePack::RPC::Client test' do
	 before(:each)do
	 	@svr,@client = start_server
	 end

	 after(:each)do
		@svr.stop
		@client.close
	 end


	 it 'should return "ok" value ' do
	    @client.call(:hello).should include("ok")
	 end
	 
	 it 'should return "3" value ' do
	    @client.call(:sum,1,2).should equal 3
	 end
	 
	 it 'should return "ok" and "3" when you call with call_async' do
	    req1 = @client.call_async(:hello)
	    req2 = @client.call_async(:sum,1,2)
	    req1.join
	    req1.result.should include("ok")
	    req1.error.should be_nil

	    req2.join
	    req2.result.should equal 3
	    req2.error.should be_nil
	 end

	 it 'should return "ok" when you set callback(:hello)' do
	    @client.callback(:hello) do |error, result|
	    	result.should include("ok")
		error.should be_nil
	    end
	 end

	 it "should return "3" when you set callback(:sum)' do

	    @client.callback(:sum) do |error, result|
	    	result.shouble equal 3
		error.should be_nil
	    end
	 end

	 it 'should return nil values when you call notify' do
	    @client.notify(:hello).should be_nil
	    @client.notify(:sum,1,2).should be_nil
	 end

	 it 'should return error when you call private method' do
	    lambda{@client.call(:hidden)}.should raise_error(MessagePack::RPC::RemoteError)
	 end

	 it 'should be throw exception message when you call exception method' do
	    lambda{@client.call(:exception)}.should raise_error(MessagePack::RPC::RemoteError,"raised")
	 end

	 it 'should be return "async" when you call with :async parameter' do
	    @client.call(:async).should include("async")

	 end


	 it 'should throws exception when you call with async_exception' do
	    lambda{@client.call(:async_exception)}.should raise_error(MessagePack::RPC::RemoteError,"async")
	 end

	 it 'should be returns correct values when you use MessagePack::RPC::SessionPool' do
	    sp = MessagePack::RPC::SessionPool.new
	    s = sp.get_session('127.0.0.1', @client.port)
	   
	   s.call(:hello).should include("ok")
	   s.call(:sum,1,2).should equal 3

	   sp.close

	 end


end

describe "MessagePack::RPC::TimeoutError test"  do

	 before(:each)do
	    @client = start_client
	    @lsock = TCPServer.new("0.0.0.0",@client.port)
	    @client.timeout = 1
	 end
	 
	 it 'should return MessagePack::RPC::TimoutError' do
	    lambda{@client.call(:hello)}.should raise_error(MessagePack::RPC::TimeoutError)
	 end	 

	 after(:all)do
	 	     @client.close
		     @lsock.close
	 end

end

describe "MessagePack::RPC::Loop testing" do
	 before(:all) do
		@loop = MessagePack::RPC::Loop.new

		@svr = MessagePack::RPC::Server.new(@loop)
		@svr.listen("0.0.0.0", MyServer.port, MyServer.new(@svr))

		@cli = MessagePack::RPC::Client.new("127.0.0.1", MyServer.port, @loop)
		@cli.timeout = 10

	end

	 it "should return correct values when you use MessagePack::RPC::Loop" do
		count = 0

		@cli.callback(:hello) do |error, result|
			result.should include("ok")
			error.should be_nil
		end

		@cli.callback(:sum, 1, 2) do |error, result|
			result.should equal 3
			error.should be_nil
		end

	 end


	after(:all) do
		@cli.close
		@svr.close
	end

end

