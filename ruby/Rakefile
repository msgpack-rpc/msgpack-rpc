require 'rake'
require 'rake/testtask'
require 'rake/clean'

begin
	require 'jeweler'
	Jeweler::Tasks.new do |gemspec|
		gemspec.name = "msgpack-rpc"
		gemspec.summary = "MessagePack-RPC, asynchronous RPC library using MessagePack"
		gemspec.author = "FURUHASHI Sadayuki"
		gemspec.email = "frsyuki@users.sourceforge.jp"
		gemspec.homepage = "http://msgpack.org/"
		gemspec.rubyforge_project = "msgpack"
		gemspec.has_rdoc = true
		gemspec.require_paths = ["lib"]
		gemspec.add_dependency "msgpack", ">= 0.4.4"
		gemspec.add_dependency "cool.io", ">= 1.0.0"
		gemspec.test_files = Dir["test/test_*.rb"]
		gemspec.files = Dir["lib/**/*", "ext/**/*", "test/**/*", "spec/**/*", "tasks/**/*"] + %w[AUTHORS ChangeLog NOTICE README]
		gemspec.add_development_dependency('rspec')
	end
	Jeweler::GemcutterTasks.new
rescue LoadError
	puts "Jeweler not available. Install it with: gem install jeweler"
end

VERSION_FILE = "lib/msgpack/rpc/version.rb"

file VERSION_FILE => ["VERSION"] do |t|
	version = File.read("VERSION").strip
	File.open(VERSION_FILE, "w") {|f|
		f.write <<EOF
module MessagePack
module RPC

VERSION = '#{version}'

end
end
EOF
	}
end

task :default => [VERSION_FILE, :build]

task :test => ['test:unit','spec:unit']
load 'tasks/msgpack_rpc_tasks.rake'
