require File.join(File.expand_path(File.dirname(__FILE__)), 'spec_helper.rb')
require File.join(File.expand_path(File.dirname(__FILE__)), 'my_server.rb')
include MyServerTest

describe 'MessagePack::RPC::Client' do
	 before(:each)do
	 	@svr,@client = start_server
		
	 end

	 after(:each)do
		@svr.stop
		@client.close
		
		
	 end


	 it 'call(:hello) method return "ok" value' do
	    @client.call(:hello).should include("ok")
	 end
	 
	 it 'call(:sum,1,2) method return "3" value' do
	    @client.call(:sum,1,2).should equal 3
	 end
	 
	 it 'call_async(:hello) and call_async(:sum,1,2)' do
	    req1 = @client.call_async(:hello)
	    req2 = @client.call_async(:sum,1,2)
	    req1.join
	    req1.result.should include("ok")
	    req1.error.should be_nil

	    req2.join
	    req2.result.should equal 3
	    req2.error.should be_nil
	 end

	 it 'callback test about string' do
	    @client.callback(:hello) do |error, result|
	    	result.should include("ok")
		error.should be_nil
	    end
	 end

	 it 'callback test about function' do

	    @client.callback(:sum) do |error, result|
	    	result.shouble equal 3
		error.should be_nil
	    end
	 end

	 it 'notify test about string' do
	    @client.notify(:hello).should be_nil
	    @client.notify(:sum,1,2).should be_nil
	 end

	 it 'should be rejected by server  ' do
	    lambda{@client.call(:hidden)}.should raise_error(MessagePack::RPC::RemoteError)
	 end

	 it 'should be throw exception message' do
	    lambda{@client.call(:exception)}.should raise_error(MessagePack::RPC::RemoteError,"raised")
	 end

	 it 'should be worked with async' do
	    @client.call(:async).should include("async")

	 end


	 it 'should throws exception with async' do
	    lambda{@client.call(:async_exception)}.should raise_error(MessagePack::RPC::RemoteError,"async")
	 end

	 it 'should be works with session pool' do
	    sp = MessagePack::RPC::SessionPool.new
	    s = sp.get_session('127.0.0.1', @client.port)
	   
	   s.call(:hello).should include("ok")
	   s.call(:sum,1,2).should equal 3

	   sp.close

	 end


end

describe "timeout check"  do

	 before(:each)do
	    @client = start_client
	    @lsock = TCPServer.new("0.0.0.0",@client.port)
	    @client.timeout = 1

	 end
	 
	 it 'should be timeouted' do

	    lambda{@client.call(:hello)}.should raise_error(MessagePack::RPC::TimeoutError)

	 end	 

	 after(:all)do
	 	     @client.close
		     @lsock.close
	 end


end

