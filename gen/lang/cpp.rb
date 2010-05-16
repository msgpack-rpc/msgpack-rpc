
def generate(doc, outdir, langdir)
	Mplex.write("#{langdir}/cpp/types.mpl", "#{outdir}/types.hpp", doc)
	doc.services.each do |s|
		Mplex.write("#{langdir}/cpp/header.mpl", "#{outdir}/#{s.name}.hpp", s)
		Mplex.write("#{langdir}/cpp/source.mpl", "#{outdir}/#{s.name}.cpp", s)
	end
	Mplex.write("#{langdir}/cpp/rpc.mpl", "#{outdir}/rpc.hpp", doc)
end

class AST::Type
	@@typemap = {
		:bool   => 'bool',
		:byte   => 'int8_t',
		:i16    => 'int16_t',
		:i32    => 'int32_t',
		:i64    => 'int64_t',
		:double => 'double',
		:string => 'std::string',
		:binary => 'msgpack::type::raw_ref',
		:slist  => '',   # FIXME
		:list   => 'std::vector',
		:set    => 'std::set',
		:map    => 'std::map',
	}
	def to_s
		if map = @@typemap[@name]
			map
		else
			@name.to_s
		end
	end
end

module AST::Util
	def expand_template(*types)
		#"<#{types.join(',').gsub('>>','> >')}>"
		"<#{types.join(',')}> "
	end
end

class AST::ListType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@element)
	end
end

class AST::SetType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@element)
	end
end

class AST::MapType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@key, @value)
	end
end

class AST::Field
	def to_s
		"#{type} #{name}"
	end
end

