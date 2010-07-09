#!/usr/bin/env ruby
require 'chukan'
include Chukan::Test

def gen(file, lang)
	system("ruby -I.. ../mprpcgen.rb #{file} -g #{lang}")
	if $? != 0
		raise "gen failed"
	end
	true
end

test "gen cpp" do
	gen "exception.mprpc", :cpp
end

test "gen ruby" do
	gen "exception.mprpc", :ruby
end

test "gen java" do
	gen "exception.mprpc", :java
end

