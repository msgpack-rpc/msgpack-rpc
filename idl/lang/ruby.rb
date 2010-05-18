require 'fileutils'

def generate(doc, outdir, langdir)
	Mplex.write("#{langdir}/ruby/types.mpl", "#{outdir}/types.rb", doc)
	doc.services.each do |s|
		Mplex.write("#{langdir}/ruby/client.mpl", "#{outdir}/#{s.name}_client.rb", s)
		Mplex.write("#{langdir}/ruby/server.mpl", "#{outdir}/#{s.name}_server.rb", s)
	end
end

module AST::Util
	def modname(name)
		name = name.to_s
		name[0..0].upcase + name[1..-1]
	end
end

class AST::ConstValue
	def to_s
		modname(value)
	end
end

class AST::StringValue
	def to_s
		value.dump
	end
end

class AST::IntValue
	def to_s
		value.to_s
	end
end

class AST::FloatValue
	def to_s
		value.to_s
	end
end

class AST::BoolValue
	def to_s
		value ? "true" : "false"
	end
end

class AST::ListValue
	def to_s
		"[#{value.map{|e|"#{e}"}.join(',')}]"
	end
end

class AST::MapValue
	def to_s
		"{#{value.map{|k,v|"#{k} => #{v}"}.join(',')}}"
	end
end

