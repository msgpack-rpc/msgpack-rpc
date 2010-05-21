require 'fileutils'

def generate(doc, outdir, langdir)
	nss = doc.namespace(:java)
	nspath = File.join(outdir, *nss)
	FileUtils.mkdir_p(nspath)

	doc.each do |d|
		case d
		when AST::Const
			#FIXME
			#Mplex.write("#{langdir}/java/const.mpl", "#{nspath}/#{d.name}.java", doc)
		when AST::Typedef
			#FIXME
			#Mplex.write("#{langdir}/java/typedef.mpl", "#{nspath}/#{d.name}.java", doc)
		when AST::Enum
			#FIXME
			#Mplex.write("#{langdir}/java/enum.mpl", "#{nspath}/#{d.name}.java", doc)
		when AST::Struct
			Mplex.write("#{langdir}/java/struct.mpl", "#{nspath}/#{d.name}.java", d)
		when AST::Exception
			#Mplex.write("#{langdir}/java/exception.mpl", "#{nspath}/#{d.name}.java", doc)
		when AST::Service
			#Mplex.write("#{langdir}/java/server.mpl", "#{nspath}/#{d.name}Server.java", doc)
			#Mplex.write("#{langdir}/java/client.mpl", "#{nspath}/#{d.name}Client.java", doc)
		end
	end
end

class AST::Type
	@@typemap = {
		'int8'   => 'byte',
		'int16'  => 'short',
		'int32'  => 'int',
		'int64'  => 'long',
		'uint8'  => 'byte',
		'uint16' => 'short',
		'uint32' => 'int',
		'uint64' => 'long',
		'bool'   => 'boolean',
		'double' => 'double',
		'bytes'  => 'byte[]',
		'string' => 'String',
		'list'   => 'List',
		'set'    => 'Set',
		'map'    => 'Map',
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
		super+expand_generics(element_type)
	end
end

class AST::SetType
	def to_s
		super+expand_generics(element_type)
	end
end

class AST::MapType
	def to_s
		super+expand_generics(key_type, value_type)
	end
end

module AST::Util
	def expand_generics(*types)
		"<#{types.join(',')}>"
	end
end

class AST::ConstValue
	def to_s
		value
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

class AST::StringValue
	def typename
		"String"
	end
end

class AST::IntValue
	def typename
		"int"
	end
end

class AST::FloatValue
	def typename
		"double"
	end
end

class AST::BoolValue
	def typename
		"boolean"
	end
end

class AST::ListValue
	def typename
		"List"
	end
end

class AST::MapValue
	def typename
		"Map"
	end
end

