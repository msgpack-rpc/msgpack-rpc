
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
		'int8'   => 'int8_t',
		'int16'  => 'int16_t',
		'int32'  => 'int32_t',
		'int64'  => 'int64_t',
		'uint8'  => 'uint8_t',
		'uint16' => 'uint16_t',
		'uint32' => 'uint32_t',
		'uint64' => 'uint64_t',
		'bool'   => 'bool',
		'double' => 'double',
		'bytes'  => 'msgpack::type::raw_ref',
		'string' => 'std::string',
		'list'   => 'std::vector',
		'set'    => 'std::set',
		'map'    => 'std::map',
	}
	def to_s
		if map = @@typemap[@name]
			map
		else
			name.to_s
		end
	end
end

class AST::ListType
	def to_s
		super+expand_template(element_type)
	end
end

class AST::SetType
	def to_s
		super+expand_template(element_type)
	end
end

class AST::MapType
	def to_s
		super+expand_template(key_type, value_type)
	end
end

module AST::Util
	def expand_template(*types)
		#"<#{types.join(',').gsub('>>','> >')}>"
		"<#{types.join(',')}> "
	end
end

class AST::Field
	def to_s
		"#{type} #{name}"
	end
end

