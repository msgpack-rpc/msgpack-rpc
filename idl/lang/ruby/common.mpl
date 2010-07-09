__BEGIN__
def modname(str)
	str[0..0].upcase + str[1..-1]
end

class AST::BuiltInType
	@@typemap = {
		'int8'   => 'Integer',
		'int16'  => 'Integer',
		'int32'  => 'Integer',
		'int64'  => 'Integer',
		'uint8'  => 'Integer',
		'uint16' => 'Integer',
		'uint32' => 'Integer',
		'uint64' => 'Integer',
		'bool'   => 'Float',
		'double' => 'Float',
		'bytes'  => 'String',
		'string' => 'String',
		'list'   => 'Array',
		'set'    => 'Array',  # FIXME
		'map'    => 'Hash',
	}

	def to_s
		@@typemap[type_name]
	end
end

class AST::ExternalType
	def to_s
		@type_name.to_s
	end
end

class AST::ConstName
	def to_s
		self[0..0].upcase + self[1..-1]
	end
end

class AST::EnumFieldName
	def to_s
		self[0..0].upcase + self[1..-1]
	end
end

class AST::BoolLiteral
	def to_s
		value ? "true" : "false"
	end
end

class AST::ListLiteral
	def to_s
		"[#{value.map{|e|"#{e}"}.join(',')}]"
	end
end

class AST::MapLiteral
	def to_s
		"{#{value.map{|k,v|"#{k} => #{v}"}.join(',')}}"
	end
end

def xjoin(array=self, sep = ', ', &block)
	block ||= Proc.new {|a| a }
	if array.is_a?(Hash)
		array = array.values
	end
	array.map(&block).join(sep)
end
__END__

%def gen_package(doc, &block)
%if nss = doc.namespace(:ruby)
%nss = nss.map {|x| modname(x) }
module {{ns}}  %|ns| nss.each
%block.call
end  # module {{ns}}  %|ns| nss.reverse.each
%else
%block.call
%end
%end

%def default_field(f)
	%v = f.default
	%if f.default.nil?
	%	v = "#{f.type.type_name}.new"
	%end
	@{{f.field_name}} = {{v}}
%end


%def gen_struct(name, fields)
	%fields.each_value do |f|
	attr_accessor :{{f.field_name}}
	%end

	def initialize
		%fields.each_value do |f|
		%	default_field(f)
		%end
	end

	def to_msgpack(out = '')
		%array = Array.new(fields.max_id)
		%1.upto(fields.max_id) do |i|
		%	if f = fields[i]
		%		array[i-1] = "@#{f.field_name}"
		%	else
		%		array[i-1] = "nil"
		%	end
		%end
		[{{xjoin(array)}}].to_msgpack(out)
	end

	def self.from_msgpack(obj)
		raise "type error" unless obj.is_a?(Array)
		raise "type error" if obj.size < {{fields.max_required_id}}

		%1.upto(fields.max_id) do |i|
		%if f = fields[i]

		%if f.required?
			%if f.type.builtin_type?
			@{{f.field_name}} = obj[{{i-1}}]
			%else
			@{{f.field_name}} = {{f.type}}.from_msgpack(obj[{{i-1}}])
			%end
		%else
			o = obj[{{i-1}}]
			%if f.type.builtin_type?
			@{{f.field_name}} = o unless o.nil?
			%else
			@{{f.field_name}} = {{f.type}}.from_msgpack(o) unless o.nil?
			%end
		%end

		%end
		%end
	end
%end
