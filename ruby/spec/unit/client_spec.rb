require File.join(File.expand_path(File.dirname(__FILE__)), 'spec_helper.rb')
require File.join(File.expand_path(File.dirname(__FILE__)), 'my_server.rb')

describe 'MessagePack::RPC::Client' do
	 before(:all)do
		puts 'server,client start'
	 	@svr,@client = MyServer.start_server

	 end

	 it 'call(:hello) method return "ok" value' do
	    @client.call(:hello).should include("ok")
	 end
	 
	 it 'call(:sum,1,2) method return "3" value' do
	    @client.call(:sum,1,2).should == 3
	 end
	 
	 it 'call(:hello) and call(:sum,1,2) with async' do
	    req1 = @client.call_async(:hello)
	    req2 = @client.call_async(:sum,1,2)
	    req1.join
	    req1.result.should include("ok")
	    req1.error.should be_nil

	    req2.join
	    req2.result.should == 3
	    req2.error.should be_nil

	 end

end