#!/usr/bin/env ruby
require 'chukan'
include Chukan::Test

def gen(file, lang)
	system("ruby -I.. ../mprpcgen.rb --verbose #{file} -g #{lang}")
	if $? != 0
		raise "gen failed"
	end
	true
end

test "test" do
	gen "exception.mprpc", :cpp
end

