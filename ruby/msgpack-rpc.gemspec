Gem::Specification.new do |s|
  s.platform = Gem::Platform::RUBY
  s.name = "msgpack-rpc"
  s.version = "0.4.0"
  s.summary = "MessagePack-RPC, asynchronous RPC library using MessagePack"
  s.author = "FURUHASHI Sadayuki"
  s.email = "frsyuki@users.sourceforge.jp"
  s.homepage = "http://msgpack.sourceforge.jp/"
  s.rubyforge_project = "msgpack"
  s.has_rdoc = false
  #s.extra_rdoc_files = ["README"]
  s.require_paths = ["lib"]
  s.add_dependency "msgpack", ">= 0.3.1"
  s.add_dependency "rev", ">= 0.3.0"
	s.test_files = Dir["test/test_*.rb"]
  s.files = Dir["lib/**/*", "ext/**/*", "test/**/*"] + %w[AUTHORS ChangeLog NOTICE README]
end

