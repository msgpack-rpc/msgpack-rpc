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

class AST::Field
	def camelname
		name[0,1].upcase + name[1..-1]
	end

	def unpack_func
		"unpack#{camelname}"
	end

	def default_func
		"default#{camelname}"
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

	@@convertmap = {
		'int8'   => 'ByteSchema.convertByte',
		'int16'  => 'ShortSchema.convertShort',
		'int32'  => 'IntSchema.convertInt',
		'int64'  => 'LongSchema.convertLong',
		'uint8'  => 'ByteSchema.convertByte',
		'uint16' => 'ShortSchema.convertShort',
		'uint32' => 'IntSchema.convertInt',
		'uint64' => 'LongSchema.convertLong',
		'bool'   => 'BooleanSchema.convertBoolean',
		'double' => 'DoubleSchema.convertDouble',
		'bytes'  => 'BytesSchema.convertBytes',
		'string' => 'StringSchema.convertStrong',
		'list'   => 'ArraySchema.convertList',
		'set'    => 'ArraySchema.convertSet',
		'map'    => 'MapSchema.convertMap',
	}

	def convert_func
		@@convertmap[@name]
	end

	@@schemamap = {
		'int8'   => 'ByteSchema',
		'int16'  => 'ShortSchema',
		'int32'  => 'IntSchema',
		'int64'  => 'LongSchema',
		'uint8'  => 'ByteSchema',
		'uint16' => 'ShortSchema',
		'uint32' => 'IntSchema',
		'uint64' => 'LongSchema',
		'bool'   => 'BooleanSchema',
		'double' => 'DoubleSchema',
		'bytes'  => 'BytesSchema',
		'string' => 'StringSchema',
		'list'   => 'ArraySchema',
		'set'    => 'ArraySchema',
		'map'    => 'MapSchema',
	}

	@@convertmap = {
		'int8'   => 'convertByte',
		'int16'  => 'convertShort',
		'int32'  => 'convertInt',
		'int64'  => 'convertLong',
		'uint8'  => 'convertByte',
		'uint16' => 'convertShort',
		'uint32' => 'convertInt',
		'uint64' => 'convertLong',
		'bool'   => 'convertBoolean',
		'double' => 'convertDouble',
		'bytes'  => 'convertBytes',
		'string' => 'convertString',
		'list'   => 'convertArray',
		'set'    => 'convertArray',
		'map'    => 'convertMap',
	}

	def new_schema
		if list?
			"new ListSchema(#{element_type.new_schema})"
		elsif set?
			"new SetSchema(#{element_type.new_schema})"
		elsif map?
			"new MapSchema(#{key_type.new_schema}, #{value_type.new_schema})"
		elsif schema = @@schemamap[@name]
			"new #{schema}()"
		else
			"new ConvertableSchema(new #{@name}())"
		end
	end

	def convert_schema(f, obj)
		if list?
			"this.#{f.name} = ListSchema.convertList(#{obj}, #{element_type.new_schema}, null);"
		elsif set?
			"this.#{f.name} = SetSchema.convertSet(#{obj}, #{element_type.new_schema}, null);"
		elsif map?
			"this.#{f.name} = MapSchema.convertMap(#{obj}, #{key_type.new_schema}, #{value_type.new_schema}, null);"
		elsif schema = @@schemamap[@name]
			"this.#{f.name} = #{schema}.#{@@convertmap[@name]}(#{obj});"
		else
			"this.#{f.name} = new #{f.type}();\n"+
			"this.#{f.name}.messageConvert(#{obj});"
		end
	end
end

class AST::ListType
	def type
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

class AST::BytesValue
	def typename
		"byte[]"
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

