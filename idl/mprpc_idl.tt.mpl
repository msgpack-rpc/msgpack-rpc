%def keyword(word, name = "k_#{word}")
	rule [%name%]
		'[%word%]' word_spacing
		{
			def text
				text_value.rstrip
			end
		}
	end
%end

%def separator(sym, name)
	rule [%name%]
		"[%sym%]" spacing %>if  sym.include?("'")
		'[%sym%]' spacing %>if !sym.include?("'")
		{
			def text
				text_value.rstrip
			end
		}
	end
%end


grammar MessagePackIDL
	####
	## Document
	##
	rule document
		spacing h:header* d:definition* spacing {
			def ast
				headers     = h.elements.map {|n| n.ast }
				definitions = d.elements.map {|n| n.ast }
				AST::Document.new(headers + definitions)
			end
		}
	end

	####
	## Header
	##
	rule header
		namespace / lang_header
	end

	## Namespace
	rule namespace
		k_namespace scope:namespace_scope name:namespace_id {
			def ast
				AST::Namespace.new(scope.text, name.text)
			end
		}
	end

	rule namespace_scope
		k_star / k_cpp / k_java / k_py / k_perl / k_php / k_rb / k_csharp
	end

	rule namespace_id
		[a-zA-Z\_\.]+ word_spacing {
			def text
				text_value.rstrip
			end
			def symbol
				text.symbol
			end
		}
	end

	## Language specific header
	rule lang_header
		cpp_include
	end

	# C++ include
	rule cpp_include
		k_cpp_include name:literal_string {
			def ast
				AST::CppInclude.new(name.text)
			end
		}
	end


	####
	## Definition
	##
	rule definition
		const / typedef / enum / senum_thrift / struct / exception / service
	end

	rule const
		k_const type:field_type id k_equal literal {
			def ast
				AST::Const.new(type.type, id.symbol, literal.value)
			end
		}
	end

	rule typedef
		k_typedef type:builtin_type id {
			def ast
				AST::Typedef.new(type.type, id.symbol)
			end
		}
	end

	rule enum
		k_enum id k_lwing fs:enum_field* k_rwing {
			def ast
				ffs = fs.elements.map {|f| f.ast }
				AST::Enum.new(id.symbol, ffs)
			end
		}
	end

	rule enum_field
		id val:(k_equal i:literal_int)? list_separator? {
			def ast
				n = val.respond_to?(:i) ? val.i.number : nil
				AST::EnumField.new(id.symbol, n)
			end
		}
	end

	rule senum_thrift
		# FIXME?
		k_senum id k_lwing (literal list_separator?)* k_rwing {
			def ast
				# FIXME
			end
		}
	end

	rule struct
		k_struct id k_lwing fs:field* k_rwing {
			def ast
				ffs = fs.elements.map {|f| f.ast }
				AST::Struct.new(id.symbol, ffs)
			end
		}
	end

	rule exception
		k_exception id k_lwing fs:field* k_rwing {
			def ast
				ffs = fs.elements.map {|f| f.ast }
				AST::Exception.new(id.symbol, ffs)
			end
		}
	end


	####
	## Service
	##
	rule service
		k_service id ex:extends? k_lwing fs:function* k_rwing {
			def ast
				fex = ex.respond_to?(:symbol) ? fex.symbol : nil
				ffs = fs.elements.map {|f| f.ast }
				AST::Service.new(id.symbol, fex, ffs)
			end
		}
	end

	rule extends
		k_extends id {
			def text
				id.text
			end
			def symbol
				id.symbol
			end
		}
	end

	rule function
		return_type id k_lparen fs:field* k_rparen th:throws? eol_mark {
			def ast
				frt = return_type.type
				fid = id.symbol
				ffs = fs.elements.map {|f| f.ast }
				fth = th.respond_to?(:array) ? th.array : []
				fth = AST::ThrowsList.new(fth)
				AST::Function.new(frt, fid, ffs, fth)
			end
		}
	end

	rule throws
		k_throws k_lparen ls:throws_class* k_rparen {
			def array
				ls.elements.map {|s| s.ast }
			end
		}
	end

	rule throws_class
		num:field_id? id eol_mark {
			def ast
				fnum  = num.respond_to?(:number) ? num.number : nil
				fname = id.text
				AST::ThrowsClass.new(fnum, fname)
			end
		}
		/ throws_class_thrift
	end

	rule throws_class_thrift
		field {
			#warn "Thrift style exception is obsolete."
			def ast
				fnum  = num.respond_to?(:number) ? num.number : nil
				fname = id.text
				AST::ThrowsClass.new(fnum, fname)
			end
		}
	end


	####
	## Messaage
	##
	rule field
		num:field_id? qu:field_qualifier? field_type id val:default_value? eol_mark {
			def ast
				fnum  = num.respond_to?(:number) ? num.number : nil
				fqu   = qu.respond_to?(:text) ? qu.text : nil
				ft    = field_type.type
				fname = id.text
				fval  = val.respond_to?(:value) ? val.value : nil
				AST::Field.new(fnum, fqu, ft, fname, fval)
			end
		}
	end

	rule field_id
		literal_uint k_colon {
			def number
				literal_uint.number
			end
		}
	end

	rule field_qualifier
		k_required / k_optional
	end

	rule default_value
		k_equal literal {
			def value
				literal.value
			end
		}
	end


	####
	## Type
	##
	rule builtin_type
		base_type { def type; AST::Type.new(text); end }
		/ container_type
	end

	rule field_type
		builtin_type
		/ id { def type; AST::Type.new(text); end }
	end

	rule return_type
		field_type
		/ k_void { def type; Type.new(text); end }
	end

	rule base_type
		k_int8 / k_int16 / k_int32 / k_int64
		/ k_uint8 / k_uint16 / k_uint32 / k_uint64
		/ k_bool / k_double
		/ k_bytes / k_string
		/ base_type_thrift
	end

	rule base_type_thrift
		k_byte / k_i16 / k_i32 / k_i64 / k_binary
	end

	rule container_type
		map_type / set_type / list_type
	end

	rule map_type
		# FIXME cpp_type?
		k_map k_lpoint k:field_type k_comma v:field_type k_rpoint {
			def type
				AST::MapType.new(k.type, v.type)
			end
		}
	end

	rule set_type
		# FIXME cpp_type?
		k_set k_lpoint e:field_type k_rpoint {
			def type
				AST::SetType.new(e.type)
			end
		}
	end

	rule list_type
		# FIXME cpp_type?
		k_list k_lpoint e:field_type k_rpoint {
			def type
				AST::ListType.new(e.type)
			end
		}
	end


	####
	## Literal
	##
	rule literal
		id {
			def value
				AST::ConstValue.new(symbol);
			end
		}
		/ literal_string / literal_int / literal_float
		/ literal_bool / literal_list / literal_map
	end

	rule literal_string
		'"' val:(!'"' . )* '"' spacing {
			def text
				val.text_value.rstrip
			end
			def value
				AST::StringValue.new(text)
			end
		}
		/
		"'" val:(!"'" . )* "'" spacing {
			def text
				val.text_value.rstrip
			end
			def value
				AST::StringValue.new(text)
			end
		}
	end

	rule literal_int
		[\+\-]? (decimal_int / hex_int / octal_int) word_spacing {
			def number
				text_value.rstrip.to_i
			end
			def value
				AST::IntValue.new(number)
			end
		}
	end

	rule literal_uint
		(decimal_int / hex_int / octal_int) word_spacing {
			def number
				text_value.rstrip.to_i
			end
			def value
				AST::IntValue.new(number)
			end
		}
	end

	rule decimal_int
		(([1-9] [0-9]*) / '0')
	end

	rule hex_int
		('0x' / '0X') [a-fA-F0-9]+
	end

	rule octal_int
		'0'  [0-7]+
	end

	rule literal_float
		# FIXME
		[\+\-]? [0-9]* ('.' [0-9]+)? ([Ee] literal_uint)? {
			def number
				text_value.rstrip.to_f
			end
			def value
				AST::FloatValue.new(number)
			end
		}
	end

	rule literal_bool
		k_false  { def value; AST::BoolValue.new(false); end }
		/ k_true { def value; AST::BoolValue.new(true);  end }
	end

	rule literal_list
		k_lbracket ls:(e:literal list_separator?)* k_rbracket {
			def value
				fls = ls.elements.map {|n| n.e.value }
				AST::ListValue.new(fls)
			end
		}
	end

	rule literal_map
		k_lwing ls:(k:literal k_colon v:literal list_separator?)* k_rwing {
			def value
				fls = {}
				ls.elements.each {|n| fls[n.k.value] = n.v.value }
				AST::MapValue.new(fls)
			end
		}
	end


	####
	## Basic symbol
	##
	rule id
		([a-zA-Z] / '_') id_char* word_spacing {
			def text
				text_value.rstrip
			end
			def symbol
				text.to_sym
			end
		}
	end

	rule id_char
		[a-zA-Z] / [0-9] / '.' / '_'
	end


	####
	## Whitespace
	##
	rule comment
		embed_comment / line_comment
	end

	rule embed_comment
		c_comment
	end

	rule line_comment
		cpp_comment / unix_comment
	end

	rule c_comment
		'/*' (!'*/' . )* '*/'
	end

	rule cpp_comment
		'//' (!"\n" . )* "\n"
	end

	rule unix_comment
		'#' (!"\n" . )* "\n"
	end

	rule spacing
		([ \t\r\n] / comment)*
	end

	rule word_spacing
		!id_char spacing
	end

	rule list_separator
		k_comma / k_semi
	end

	rule eol_mark
		(k_comma / k_semi)?
	end


	% keyword('include')
	% keyword('namespace')

	% keyword('cpp')
	% keyword('java')
	% keyword('py')
	% keyword('perl')
	% keyword('php')
	% keyword('rb')
	% keyword('csharp')
	% keyword('cpp_include')

	% keyword('const')
	% keyword('typedef')
	% keyword('enum')
	% keyword('struct')
	% keyword('senum')
	% keyword('exception')
	% keyword('service')

	% keyword('extends')
	% keyword('throws')
	% keyword('required')
	% keyword('optional')

	% keyword('void')
	% keyword('int8')
	% keyword('int16')
	% keyword('int32')
	% keyword('int64')
	% keyword('uint8')
	% keyword('uint16')
	% keyword('uint32')
	% keyword('uint64')
	% keyword('bool')
	% keyword('double')
	% keyword('bytes')
	% keyword('string')

	% keyword('byte')
	% keyword('binary')
	% keyword('i16')
	% keyword('i32')
	% keyword('i64')

	% keyword('map')
	% keyword('set')
	% keyword('list')

	% keyword('true')
	% keyword('false')

	% separator('*', :k_star)
	% separator('=', :k_equal)
	% separator('{', :k_lwing)
	% separator('}', :k_rwing)
	% separator('(', :k_lparen)
	% separator(')', :k_rparen)
	% separator(':', :k_colon)
	% separator(',', :k_comma)
	% separator(';', :k_semi)
	% separator('<', :k_lpoint)
	% separator('>', :k_rpoint)
	% separator('[', :k_lbracket)
	% separator(']', :k_rbracket)
end

# vim: ft=treetop
