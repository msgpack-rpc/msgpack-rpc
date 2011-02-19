# desc "Explaing what task does"

begin
  require 'spec'
  require 'spec/rake/spectask'
  namespace :spec do
    desc 'run unit core testing'
    Spec::Rake::SpecTask.new(:unit) do |t|
      spec_dir = File.join(File.dirname(__FILE__), '..', 'spec')
      t.spec_opts = File.read(File.join(spec_dir, 'spec.opts')).split
      t.spec_files = FileList[File.join(spec_dir, 'unit', '**', '*_spec.rb')]
    end
  end
rescue LoadError
  warn "Rspec is not installed. Please install Rspec with gems and --pre"
end

namespace :test do
	  desc "run test"
	  Rake::TestTask.new(:unit) do |t|
	  t.libs << 'lib'
	  t.pattern = 'test/*_test.rb'
	  t.verbose = true
	  end
end