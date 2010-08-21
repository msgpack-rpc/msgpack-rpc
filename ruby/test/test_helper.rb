begin
require 'rubygems'
rescue LoadError
end
require 'test/unit'
$LOAD_PATH.unshift File.dirname(__FILE__)+'/../lib'
require 'msgpack/rpc'
