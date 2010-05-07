require 'lex'
require 'parse'
require 'ast'
require 'mplex'

if ARGV.size != 2
	prog = File.basename($0)
	puts "usage: #{prog} <mpl> <src>"
	puts ""
	puts "example:"
	puts "  #{prog} cpp.mpl test.thrift"
	exit 1
end

mpl = File.read(ARGV[0])
src = File.read(ARGV[1])

tokens = Lex.scan(src)
#require 'pp'
#pp tokens

parser = Parse.new
ast = parser.parse(tokens)
#p ast

out = Mplex.result(mpl, ast, ARGV[0])
puts out

