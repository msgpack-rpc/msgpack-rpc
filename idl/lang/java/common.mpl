__BEGIN__

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
		'void'   => 'void'
	}

	def to_s
		if map = @@typemap[@name]
			map
		else
			name.to_s
		end
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
		'bytes'  => 'ByteArraySchema',
		'string' => 'StringSchema',
		'list'   => 'ArraySchema',
		'set'    => 'ArraySchema',
		'map'    => 'MapSchema',
	}

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
		'bytes'  => 'ByteArraySchema.convertByteArray',
		'string' => 'StringSchema.convertString',
		'list'   => 'ArraySchema.convertList',
		'set'    => 'ArraySchema.convertSet',
		'map'    => 'MapSchema.convertMap',
	}

	def new_schema
		if list_type?
			"new ListSchema(#{element_type.new_schema})"
		elsif set_type?
			"new SetSchema(#{element_type.new_schema})"
		elsif map_type?
			"new MapSchema(#{key_type.new_schema}, #{value_type.new_schema})"
		elsif schema = @@schemamap[@name]
			"new #{schema}()"
		else
			"new UserClassSchema(new #{@name}())"
		end
	end

	def convert_schema(f, obj)
		if list_type?
			"this.#{f.name} = ListSchema.convertList(#{obj}, #{element_type.new_schema}, null);"
		elsif set_type?
			"this.#{f.name} = SetSchema.convertSet(#{obj}, #{element_type.new_schema}, null);"
		elsif map_type?
			"this.#{f.name} = MapSchema.convertMap(#{obj}, #{key_type.new_schema}, #{value_type.new_schema}, null);"
		elsif schema = @@schemamap[@name]
			"this.#{f.name} = #{@@convertmap[@name]}(#{obj});"
		else
			"this.#{f.name} = new #{f.type}();\n"+
			"this.#{f.name}.messageConvert(#{obj});"
		end
	end

	protected
	def expand_generics(*types)
		"<#{types.join(',')}>"
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


def mputs(msg)
	@_mplexout.concat("\t\t#{msg}\n")
end

$anon_seqid = 0
def next_anon
	"_A#{$anon_seqid+=1}"
end

def default_field(f)
	gen_literal(f.type, f.default, "this.#{f.name}")
end

def gen_literal(type, val, name = nil)
	if name
		decl = %[#{name}]
	else
		name = next_anon
		decl = %[#{type} #{name}]
	end

	if type.bytes_type?
		mputs %[#{decl} = new byte[#{val.value.length}];]
	elsif type.string_type?
		mputs %[#{decl} = #{val.value.dump};]
	elsif type.base_type?
		mputs %[#{decl} = #{val.value};]

	elsif type.list_type?
		mputs %[#{decl} = new ArrayList(#{val.value.size});]
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.add(#{ename});]
		}

	elsif type.set_type?
		mputs %[#{decl} = new HashSet(#{val.value.size});]
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.add(#{ename});]
		}

	elsif type.map_type?
		mputs %[#{decl} = new HashMap(#{val.value.size});]
		val.value.each_pair {|k,v|
			kname = gen_literal(type.key_type, k)
			vname = gen_literal(type.value_type, v)
			mputs %[#{name}.put(#{kname}, #{vname});]
		}

	else
		mputs %[#{decl} = new #{type}();]
	end
end

def unpack_field(f)
	gen_unpack(f.type, "this.#{f.name}")
end

def gen_unpack(type, name = nil)
	if name.nil?
		name = next_anon
		decl = %[#{type} #{name}]
	else
		decl = %[#{name}]
	end

	if type.base_type?
		case type.name
		when 'int8'
			mputs %[#{decl} = _Pac.unpackByte();]
		when 'int16'
			mputs %[#{decl} = _Pac.unpackShort();]
		when 'int32'
			mputs %[#{decl} = _Pac.unpackInt();]
		when 'int64'
			mputs %[#{decl} = _Pac.unpackLong();]
		when 'uint8'
			mputs %[#{decl} = _Pac.unpackByte();]
		when 'uint16'
			mputs %[#{decl} = _Pac.unpackShort();]
		when 'uint32'
			mputs %[#{decl} = _Pac.unpackInt();]
		when 'uint64'
			mputs %[#{decl} = _Pac.unpackLong();]
		when 'double'
			mputs %[#{decl} = _Pac.unpackDouble();]
		when 'bool'
			mputs %[#{decl} = _Pac.unpackBoolean();]
		when 'bytes'
			mputs %[#{decl} = _Pac.unpackByteArray();]
		when 'string'
			mputs %[#{decl} = _Pac.unpackString();]
		end

	elsif type.external_type?
		mputs %[#{decl} = new #{type}();]
		mputs %[#{name}.messageUnpack(_Pac);]

	elsif type.list_type? || type.set_type?
		length = next_anon
		element_type = type.element_type
		mputs %[int #{length} = _Pac.unpackArray();]
		mputs %[#{decl} = new ArrayList(#{length});]  if type.list_type?
		mputs %[#{decl} = new HashSet(#{length});]    if type.set_type?
		i = next_anon
		mputs %[for(int #{i}=0; #{i} < #{length}; #{i}++) {]
			vname = gen_unpack(element_type)
			mputs %[#{name}.add(#{vname});]
		mputs %[}]

	else
		length = next_anon
		key_type = type.key_type
		value_type = type.value_type
		mputs %[int #{length} = _Pac.unpackArray();]
		mputs %[#{decl} = new HashMap(#{length});]
		i = next_anon
		mputs %[for(int #{i}=0; #{i} < #{length}; #{i}++) {]
			kname = gen_unpack(key_type)
			vname = gen_unpack(value_type)
			mputs %[#{name}.put(#{kname}, #{vname});]
		mputs %[}]
	end

	return name
end

__END__


%def gen_package(doc)
%nss = doc.namespace(:java)
package {{nss.join('.')}}; %>unless nss.empty?
%end

