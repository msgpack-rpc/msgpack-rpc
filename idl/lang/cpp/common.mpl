__BEGIN__
class AST::BuiltInType
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
		'void'   => 'void'
	}

	def to_s
		if map = @@typemap[@name]
			map
		else
			name.to_s
		end
	end

	private
	def expand_template(*types)
		"<#{types.join(',')}> "
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

class AST::Field
	def to_s
		"#{type} #{name}"
	end
end

def xjoin(array=self, sep = ', ', &block)
	block ||= Proc.new {|a| a }
	if array.is_a?(Hash)
		array = array.values
	end
	array.map(&block).join(sep)
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

	b = simple_initializable(type, val)
	if b
		mputs %[#{decl} = #{val};]
	elsif b.nil?
		return
	end

	if type.list_type?
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.push_back(#{ename});]
		}

	elsif type.set_type?
		val.value.each {|e|
			ename = gen_literal(type.element_type, e)
			mputs %[#{name}.insert(#{ename});]
		}

	elsif type.map_type?
		val.value.each_pair {|k,v|
			kname = gen_literal(type.key_type, k)
			vname = gen_literal(type.value_type, v)
			mputs %[#{name}.insert( #{type}::value_type(#{kname}, #{vname}) );]
		}

	end
end

# nil   : default constructor
# false : init function (use default_field(f))
# true  : operator= or copy constructor
def simple_initializable(type, val)
	if type.bytes_type?
		nil
	elsif type.string_type?
		if val.value.empty?
			nil
		else
			true
		end
	elsif type.base_type?
		true
	elsif type.container_type?
		if val.value.empty?
			nil
		else
			false
		end
	elsif val.nil?
		nil
	elsif val.var?
		true
	else
		nil
	end
end
__END__

%def gen_guard(fname, &block)
%guard = "MPRPC_#{fname}_#{"%08x"%rand(1<<32)}_HPP__"
#ifndef {{guard}}
#define {{guard}}
%block.call
#endif
%end

%def gen_package(doc, &block)
%if nss = doc.namespace(:cpp)
namespace {{ns}} {  %|ns| nss.each
%block.call
}  // namespace {{ns}}  %|ns| nss.reverse.each
%else
%block.call
%end
%end

%def gen_struct(name, fields)
	%simple = fields.map {|i,f|
	%	simple_initializable(f.type, f.default) ? f : nil
	%}.compact

	{{name}}()
	%unless simple.empty?
		:
		[%xjoin(simple, ",\n\t\t") {|f| "#{f.field_name}(#{f.default})" }%]
	%end
	{
		%fields.each_value {|f|
		%	unless simple.include?(f)
		%		default_field(f)
		%	end
		%}
	}

	%fields.each_value do |f|
	{{f.type}} {{f.field_name}};
	%end

	template <typename Packer>
	void msgpack_pack(Packer& _Pk) const {
		_Pk.pack_array({{fields.max_id}});
		%1.upto(fields.max_id) do |i|
		%if f = fields[i]
		_Pk.pack({{f.field_name}});
		%else
		_Pk.pack_nil();
		%end
		%end
	}

	void msgpack_unpack(msgpack::object _Obj) {
		if(_Obj.type != msgpack::type::ARRAY) {
			throw msgpack::type_error();
		}
		const size_t _Length = _Obj.via.array.size;
		msgpack::object* const _Array = _Obj.via.array.ptr;

		if(_Length < {{fields.max_required_id}}) {
			throw msgpack::type_error();
		}

		%1.upto(fields.max_id) do |i|
		%if f = fields[i]

		%if f.required?
			_Array[{{i-1}}].convert(&{{f.field_name}});
		%else
			if(_Length <= {{i-1}}) { return; }  %>if i > fields.max_required_id
			if(!_Array[{{i-1}}].is_nil()) {
				_Array[{{i-1}}].convert(&{{f.field_name}});
			}
		%end

		%end
		%end
	}
%end

