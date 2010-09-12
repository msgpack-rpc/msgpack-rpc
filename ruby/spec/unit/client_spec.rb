require File.join(File.expand_path(File.dirname(__FILE__)), 'spec_helper.rb')

describe 'MessagePack::RPC::Client' do
	 before(:each)do
		@client = MessagePack::RPC::Client.new("localhost",5000)

	 end
end