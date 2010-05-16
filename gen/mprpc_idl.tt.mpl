%def keyword(word, name = "k_#{word}")
	rule [%name%]
		'[%word%]' w_
		{
			def text
				text_value.rstrip
			end
		}
	end
%end

%def separator(sym, name)
	rule [%name%]
		"[%sym%]" _ %>if  sym.include?("'")
		'[%sym%]' _ %>if !sym.include?("'")
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
		_ h:header* d:definition* _ {
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
		include_ / namespace / lang_header
	end

	## IDL include
	rule include_
		k_include name:literal_string {
			def ast
				# FIXME
			end
		}
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
		[a-zA-Z\_\.]+ w_ {
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
				AST::CppInclude.new(name.value)
			end
		}
	end


	####
	## Definition
	##
	rule definition
		const / typedef / enum / senum / struct / exception / service
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
				n = val.respond_to?(:i) ? val.i.value : nil
				AST::EnumField.new(id.symbol, n)
			end
		}
	end

	rule senum
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
				AST::Function.new(frt, fid, ffs, fth)
			end
		}
	end

	rule throws
		# FIXME なぜfield*？
		k_throws k_lparen field* k_rparen {
			def array
				[]  # FIXME
			end
		}
	end


	####
	## Messaage
	##
	rule field
		num:field_id? qu:field_qualifier? field_type id val:default_value? eol_mark {
			def ast
				fnum  = num.respond_to?(:value) ? num.value : nil
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
			def value
				literal_uint.value
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
		k_bool / k_byte / k_i16 / k_i32 / k_i64 / k_double / k_string / k_binary / k_slist
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
		id { def value; symbol; end }
		/ literal_string / literal_int / literal_float
		/ literal_bool / literal_list / literal_map
	end

	rule literal_string
		'"' val:(!'"' . )* '"' _ {
			def value
				val.text_value.rstrip
			end
		}
		/
		"'" val:(!"'" . )* "'" _ {
			def value
				val.text_value.rstrip
			end
		}
	end

	rule literal_int
		[\+\-]? (decimal_int / hex_int / octal_int) w_ {
			def value
				text_value.rstrip.to_i
			end
		}
	end

	rule literal_uint
		(decimal_int / hex_int / octal_int) w_ {
			def value
				text_value.rstrip.to_i
			end
		}
	end

	rule decimal_int
		(([1-9] [0-9]*) / '0') {
			def value
				text_value.rstrip.to_i
			end
		}
	end

	rule hex_int
		('0x' / '0X') [a-fA-F0-9]+ {
			def value
				text_value.rstrip.to_i
			end
		}
	end

	rule octal_int
		'0'  [0-7]+ {
			def value
				text_value.rstrip.to_i
			end
		}
	end

	rule literal_float
		# FIXME
		[\+\-]? [0-9]* ('.' [0-9]+)? ([Ee] literal_uint)? {
			def value
				text_value.rstrip.to_f
			end
		}
	end

	rule literal_bool
		k_false { def value; false; end }
		/ k_true { def value; true; end }
	end

	rule literal_list
		k_lbracket (literal list_separator?)* k_rbracket
	end

	rule literal_map
		k_lwing (literal k_colon literal list_separator?)* k_rwing
	end


	####
	## Basic symbol
	##
	rule id
		([a-zA-Z] / '_') id_char* w_ {
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

	rule id_separate
		!id_char spacing
	end

	rule list_separator
		k_comma / k_semi
	end

	rule eol_mark
		(k_comma / k_semi)?
	end


	####
	## Alias
	##
	rule _
		spacing
	end

	rule w_
		id_separate
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
	% keyword('senum')
	% keyword('struct')
	% keyword('exception')
	% keyword('service')
	% keyword('extends')
	% keyword('throws')
	% keyword('required')
	% keyword('optional')
	% keyword('bool')
	% keyword('byte')
	% keyword('i16')
	% keyword('i32')
	% keyword('i64')
	% keyword('double')
	% keyword('string')
	% keyword('binary')
	% keyword('slist')
	% keyword('void')
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
