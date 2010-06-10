
module FieldFunctions

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

	if type.bytes?
		mputs %[#{decl} = new byte[#{val.value.length}];]
	elsif type.string?
		mputs %[#{decl} = #{val.value.dump};]
	elsif type.base_type?
		mputs %[#{decl} = #{val.value};]
	elsif type.list?
		mputs %[#{decl} = new ArrayList();]
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.add(#{ename});]
		}
	elsif type.set?
		mputs %[#{decl} = new HashSet();]
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.add(#{ename});]
		}
	elsif type.map?
		mputs %[#{decl} = new HashMap();]
		val.value.each_pair {|k,v|
			kname = gen_literal(type.key_type, k)
			vname = gen_literal(type.value_type, v)
			mputs %[#{name}.put(#{kname}, #{vname});]
		}
	else
		mputs %[#{decl} = new #{val.value}();]
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
	elsif type.user_type?
		mputs %[#{decl} = new #{type}();]
		mputs %[#{name}.messageUnpack(_Pac);]
	elsif type.list? || type.set?
		length = next_anon
		element_type = type.element_type
		mputs %[int #{length} = _Pac.unpackArray();]
		mputs %[#{decl} = new ArrayList(#{length});]  if type.list?
		mputs %[#{decl} = new HashSet(#{length});]    if type.set?
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

end

