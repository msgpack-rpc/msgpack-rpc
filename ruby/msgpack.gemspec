Gem::Specification.new do |s|
  #s.platform = Gem::Platform::CURRENT
  s.name = "msgpack-rpc"
  s.version = "0.1.0"
  s.summary = "MessagePack"
  s.author = "FURUHASHI Sadayuki"
  s.email = "frsyuki@users.sourceforge.jp"
  s.homepage = "http://msgpack.sourceforge.jp/"
  s.rubyforge_project = "msgpack"
  s.require_paths = ["lib"]
  s.add_dependency "msgpack", ">= 0.3.1"
  s.add_dependency "rev", ">= 0.3.0"
  s.files = ["lib/**/*", "ext/**/*"].map {|g| Dir.glob(g) }.flatten +
		%w[NOTICE README]
end

