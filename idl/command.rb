#!/usr/bin/env ruby

require 'optparse'

conf = {
	:outdir  => nil,
	:lang    => nil,
	:verbose => false,
	:devel => false,
}

op = OptionParser.new
op.on('-o', '--output DIR') {|s| conf[:outdir] = s }
op.on('-g', '--gen LANG')   {|s| conf[:lang] = s }
op.on('-v', '--verbose')    { conf[:verbose] = true }
op.on('', '--devel')        { conf[:devel] = true }
op.banner += " <input>"

(class<<self;self;end).module_eval {
	define_method(:usage) {|msg|
		puts op.to_s
		puts msg if msg
		exit 1
	}
}

op.parse!(ARGV)

begin
	op.parse!(ARGV)
	usage(nil) if ARGV.length != 1

	input = ARGV.shift

	lang = conf[:lang]
	usage("-g option is required") unless lang

	outdir = conf[:outdir]
	outdir ||= "gen-#{lang}"

	verbose = conf[:verbose]

rescue
	usage($!.to_s)
end

require 'rubygems'
require 'treetop'
require 'parser'
require 'ast'
require 'mplex'

LANGDIR = File.expand_path File.join(File.dirname(__FILE__), "lang")

begin
	require "#{LANGDIR}/#{lang}"
rescue LoadError
	usage("'#{lang}' is not supported.")
end

if input == "-"
	in_fname = "(stdin)"
	in_body  = STDIN.read
else
	in_fname = input
	in_body  = File.read(in_fname)
end


def expand_include(src, fname, body="")
	s = src.split(/(?:(?![a-zA-Z0-9_]).|\A)include[ \t\r\n]*[\'\"]([^\'\"]*)[\'\"]/)
	s.each_with_index do |m,i|
		if i % 2 == 1
			path = File.expand_path(m, File.dirname(fname))
			expand_include(File.read(path), path, body)
		else
			body << m
		end
	end
	body
end

body = expand_include(in_body, in_fname)


parser = MessagePackIDLParser.new
parser.consume_all_input = true
sn = parser.parse(body)

if sn.nil?
	puts "Parse error at #{in_fname}:#{parser.failure_line}:#{parser.failure_column}:"
	puts parser.failure_reason
	exit 1
end

doc = sn.ast
doc.normalize!(conf)

if verbose
	require 'pp'
	pp doc
end

Dir.mkdir(outdir) unless File.directory?(outdir)
outdir = File.expand_path(outdir)

generate(doc, outdir, LANGDIR)

