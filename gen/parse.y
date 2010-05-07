
class Parse
rule
	document:
		headers definitions
			{ result = Document.new(Headers.new(val[0]), Definitions.new(val[1])) }

	definitions:
		definitions definition { result = val[0]+[val[1]] }
		| { result = [] }

	headers:
		headers header { result = val[0]+[val[1]] }
		| { result = [] }

	header:
		include | cpp_include | namespace

	include:
		'include' Literal
			{ result = Include.new(val[1]) }  # FIXME

	cpp_include:
		'cpp_include' Literal
			{ result = CppInclude.new(val[1]) }

	namespace_scope:
		'*' | 'cpp' | 'java' | 'py' | 'perl' | 'php' | 'rb' | 'cocoa' | 'csharp'

	# FIXME smalltalk.category smalltalk.prefix
	generic_namespace:
		'namespace' namespace_scope Identifier
			{ result = Namespace.new(val[1], val[2]) }

	php_namespace:
		'php_namespace' Identifier
			{ result = PHPNamespace.new(val[1]) }

	namespace:
		generic_namespace | php_namespace

	definition:
		const | typedef | enum | senum | struct | exception | service

	const:
		'const' field_type Identifier '=' const_value list_separator
			{ result = Const.new(val[1], val[2], val[4]) }

	typedef:
		'typedef' definition_type Identifier
			{ result = Typedef.new(val[1], val[2]) }

	enum_default:
		'=' IntConstant { result = val[1] }
		|

	enum_field:
		Identifier enum_default list_separator
			{ result = EnumField.new(val[0], val[1]) }

	enum_fields:
		enum_fields enum_field { result = val[0]+[val[1]] }
		| { result = [] }

	enum:
		'enum' Identifier '{' enum_fields '}'
			{ result = Enum.new(val[1], val[3]) }

	senum_field:
		Literal list_separator { result = val[0] }
			{ result = SenumField.new(val[0]) }

	senum_fields:
		senum_fields senum_field { result = val[0]+[val[1]] }
		| { result = [] }

	senum:
		'senum' Identifier '{' senum_fields '}'
			{ result = Senum.new(val[1], val[3]) }

	# FIXME 'xsd_all'?
	struct:
		'struct' Identifier '{' fields '}'
			{ result = Struct.new(val[1], val[3]) }

	exception:
		'exception' Identifier '{' fields '}'
			{ result = Exception.new(val[1], val[3]) }

	extends:
		'extends' Identifier { result = val[1] }
		| { result = nil }

	service:
		'service' Identifier extends '{' functions '}'
			{ result = Service.new(val[1], val[2], val[4]) }

	functions:
		functions function { result = val[0]+[val[1]] }
		| { result = [] }

	default_parameter:
		'=' const_value { result = val[1] }
		| { result = nil }

	# FIXME XsdFieldOptions
	field:
		field_id field_req field_type Identifier default_parameter list_separator
			{ result = Field.new(val[0], val[1], val[2], val[3], val[4]) }

	field_id:
		IntConstant ':' { result = val[0] }
		| { result = nil }

	field_req:
		'required' { result = true }
		| 'optional' { result = false }
		| { result = nil }

	fields:
		fields field { result = val[0]+[val[1]] }
		| { result = [] }

	# FIXME 'oneway'?
	function:
		function_type Identifier '(' fields ')' throws list_separator
			{ result = Function.new(val[0], val[1], val[3], val[5]) }

	function_type:
		field_type { result = val[0] }
		| 'void' { result = Type.new(val[0]) }

	throws:
		'throws' '(' fields ')' { result = val[3] }
		| { result = [] }

	cpp_type:
		'cpp_type' Literal { result = val[1] }
		|

	map_type:
		'map' cpp_type '<' field_type ',' field_type '>'
			{ result = MapType.new(val[0], val[3], val[5], val[1]) }

	set_type:
		'set' cpp_type '<' field_type '>'
			{ result = SetType.new(val[0], val[3], val[1]) }

	list_type:
		'list' '<' field_type '>' cpp_type
			{ result = ListType.new(val[0], val[2], val[4]) }

	base_type:
		'bool' | 'byte' | 'i16' | 'i32' | 'i64' | 'double' | 'string' | 'binary' | 'slist'

	container_type:
		map_type | set_type | list_type

	definition_type:
		base_type { result = Type.new(val[0]) }
		| container_type

	field_type:
		Identifier { result = Type.new(val[0]) }
		| base_type { result = Type.new(val[0]) }
		| container_type { result = val[0] }

	const_value:
		IntConstant | DoubleConstant | Literal | Identifier | const_list | const_map

	const_list_value:
		const_value

	const_list_values:
		const_list_values const_list_value { result = val[0]+[val[1]] }
		| { result = [] }

	const_list:
		'[' const_list_values list_separator ']'
			{ result = val[1] }

	const_map_value:
		const_value ':' const_value list_separator
			{ result = [val[0], val[2]] }

	const_map_values:
		const_map_values const_map_value { result = val[0]+[val[1]] }
		| { result = [] }

	const_map:
		'{' const_map_values '}'

	list_separator:
		',' | ';' |
end

---- header
---- inner
include AST

def parse(tokens)
	@tokens = tokens.dup
	@pos = 0
	do_parse
end

def next_token
	if @tokens.empty?
		[false, '$end']
	else
		name, token, pos = @tokens.shift
		@pos = pos
		[name, token]
	end
end

def on_error(*args)
	$stderr.puts @pos
	super
end

# vim: ft=racc
