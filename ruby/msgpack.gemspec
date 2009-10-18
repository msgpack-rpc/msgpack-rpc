Gem::Specification.new do |s|
  s.platform = Gem::Platform::RUBY
	desc = "RPC library using MessagePack, a ainary-based efficient data interchange format."
  s.name = "msgpack-rpc"
  s.version = "0.1.2"
  s.summary = desc
	s.description = desc
  s.author = "FURUHASHI Sadayuki"
  s.email = "frsyuki@users.sourceforge.jp"
  s.homepage = "http://msgpack.sourceforge.jp/"
  s.rubyforge_project = "msgpack"
  s.require_paths = ["lib"]
  s.add_dependency "msgpack", ">= 0.3.1"
  s.add_dependency "rev", ">= 0.3.0"
	s.test_files = Dir.glob("test/test_*.rb")
  s.files = ["lib/**/*", "ext/**/*"].map {|g| Dir.glob(g) }.flatten +
		%w[NOTICE README]
end

