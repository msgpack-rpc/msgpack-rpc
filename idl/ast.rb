module AST


class Data
	def initialize(document, data)
		@document = document
		@data = data
	end

	attr_accessor :data

	attr_reader :document
	alias doc document
end


module IDNormalizable  #:nodoc:
	# Struct, Exception, ExceptionFieldMap
	def normalize_id(array, hash)
		used = []
		array.each do |f|
			if id = f.id
				used << id
			end
		end

		n = 1
		array.each do |f|
			if id = f.id
				n = id+1
			else
				while used.include?(n)
					n += 1
				end
				f.id = n
				used << n
				n += 1
			end
		end

		array.each do |f|
			hash[f.id] = f
		end

		if used.empty?
			return 0, 0
		else
			return used.first, used.last
		end
	end
end


# String class used for Constant#const_name.
class ConstName < String
end

# String class used for
# Service#extends, TypeDeclaration#type_name and Type#type_name.
class TypeName < ConstName
end

# String class used for Field#field_name.
class FieldName < String
end

# String class used for EnumField#field_name.
class EnumFieldName < FieldName
end

# String class used for Function#function_name.
class FunctionName < FieldName
end


module TopLevelDeclaration
	attr_accessor :document
	alias doc document

	def normalize!(conf)  #:nodoc:
	end
end


# Describes a IDL document.
# You can attach and get optional data using Document#data= and Document#data method.
# This class has configuration data passed by command line argument on Document#conf.
#   conf[:verbose] := enable verbose mode.
#   conf[:outdir]  := output directory name
#
class Document < Array
	def initialize(defs)  #:nodoc:
		super(defs)
		@data = {}
	end

	def normalize!(conf)  #:nodoc:
		@conf = conf
		compact!
		conf[:typedef] = {}
		each {|d| d.document = self }
		each {|d| d.normalize!(conf) }
		reject! {|d| d.class == Typedef }
	end

	attr_accessor :conf

	# Gets/Sets optional data.
	attr_accessor :data

	# Gets all Constant declarations.
	def constants
		select {|x| x.class == Constant }
	end

	# Gets all Enum declarations.
	def enums
		select {|x| x.class == Enum }
	end

	# Gets all Struct declarations.
	def structs
		select {|x| x.class == Struct }
	end

	# Gets all Exception declarations.
	def exceptions
		select {|x| x.class == Exception }
	end

	# Gets all Service declarations.
	def services
		select {|x| x.class == Service }
	end

	# Gets Namespace declaration matches for `scope` language.
	def namespace(lang)
		lang = lang.to_sym
		nss = select {|x| x.class == Namespace }
		ns = nss.select {|x| x.lang == lang }.last
		ns ||= nss.select {|x| x.lang == :"*" }.last
		ns ||= Namespace.new(lang, "")
		ns
	end

	# Gets all CppInclude declarations.
	def cpp_includes
		select {|x| x.class == CppInclude }
	end

	def document
		self
	end
	alias doc document
end


# Describes declaration of `cpp_includes`.
class CppInclude
	def initialize(name)  #:nodoc:
		@name = name.to_s
	end

	# Gets name of the header. String.
	attr_accessor :name

	def to_s
		@name.to_s
	end

	include TopLevelDeclaration
end


# Namespace is an Array of domain name.
# e.g. org.msgpack.ns => ["org", "msgpack", "ns"]
class Namespace < Array
	def initialize(lang, name)  #:nodoc:
		@lang = lang.to_sym
		@name = name.to_s
		super(name.split('.'))
	end

	LANG_NORMALIZE_MAP = {
		'rb'   => 'ruby',
		'py'   => 'python',
	}

	def normalize!(conf)  #:nodoc:
		if n = LANG_NORMALIZE_MAP[@lang]
			@lang = n
		end
	end

	def to_s
		@name.to_s
	end

	attr_accessor :lang

	attr_accessor :name

	include TopLevelDeclaration
end


# Describes declaration of a constant.
class Constant
	def initialize(type, name, value)  #:nodoc:
		@type = type
		@name = name
		@const_name = ConstName.new(name)
		@value = value
	end

	# Gets declared type. Type.
	attr_accessor :type

	# Gets declared name. ConstName.
	attr_accessor :const_name

	# Gets declared name. String.
	attr_reader :name

	# Gets value. Always a Literal, not null.
	attr_accessor :value

	def normalize!(conf)  #:nodoc:
		@type = @type.normalize_type(conf)
	end

	include TopLevelDeclaration
end


# Base class of Typedef, Enum, Struct, Exception and Service.
class TypeDeclaration
	def initialize(name)  #:nodoc:
		@name = name
		@type_name = TypeName.new(name)
	end

	def to_s
		@type_name.to_s
	end

	# Gets declared name of this type. TypeName.
	attr_accessor :type_name

	# Gets declared name of this type. String.
	attr_reader :name
end


# Describes declaration of a `typedef`.
class Typedef < TypeDeclaration
	def initialize(name, type)  #:nodoc:
		@type = type
		super(name)
	end

	# Gets declared type. Type.
	attr_accessor :type

	def normalize!(conf) #:nodoc:
		@type = @type.normalize_type(conf)
		conf[:typedef][@name] = @type
	end

	include TopLevelDeclaration
end


# Describes declaration of a `enum`.
class Enum < TypeDeclaration
	def initialize(name, enum)  #:nodoc:
		@enum = enum
		super(name)
	end
	
	# Gets list of elements. Array of EnumField.
	attr_accessor :enum

	def normalize!(conf)  #:nodoc:
		n = 0
		@enum.each do |f|
			if val = f.num
				n = val+1
			else
				f.num = n
				n += 1
			end
		end
	end

	include TopLevelDeclaration
end

class EnumField
	def initialize(name, value)  #:nodoc:
		@name = name
		@field_name = EnumFieldName.new(name)
		@num = num
	end

	# Gets declared name. FieldName.
	attr_accessor :field_name

	# Gets declared name. String.
	attr_reader :name

	# Gets the number. Always Fixnum, not null.
	attr_accessor :num
end


# Describes declaration of a `struct`.
class Struct < TypeDeclaration
	def initialize(name, fields)  #:nodoc:
		@fields = fields
		super(name)
	end

	# Gets fields. FieldMap.
	attr_accessor :fields

	def normalize!(conf)  #:nodoc:
		@fields.normalize!(conf)
	end

	def exception?
		false
	end

	include TopLevelDeclaration
end

# Describes declaration of a `exception`.
class Exception < Struct
	def initialize(name, fields)
		super(name, fields)
	end

	def exception?
		true
	end
end

class FieldMap < Hash
	def initialize(fields)  #:nodoc:
		@list = fields
		super()
	end

	include IDNormalizable  #:nodoc:

	def normalize!(conf)  #:nodoc:
		@list.each {|f| f.normalize!(conf) }
		@list.reject! {|f| f.type.void_type? }
		@min_id, @max_id = normalize_id(@list, self)
	end

	# Gets all fields declared as `optional`.
	def optionals
		Hash[ select {|i,f| f.required? } ]
	end

	# Gets all fields declared as `requred`.
	def requireds
		Hash[ select {|i,f| f.required? } ]
	end

	attr_accessor :min_id, :max_id

	def min_required_id
		required_id(:first)
	end

	def max_required_id
		required_id(:last)
	end

	private
	def required_id(method)
		array = select {|i,f| f.required? }.sort_by {|i,f| i }
		return 0 if array.empty?
		array.send(method)[0]
	end
end

class Field
	def initialize(id, qualifier, type, name, default)  #:nodoc:
		@id = id
		@qualifier = qualifier
		@type = type
		@name = name
		@field_name = FieldName.new(name)
		@default = default
	end

	def normalize!(conf)  #:nodoc:
		if !@default.nil? || @qualifier == "required"
			@required = true
		else
			@required = false
		end
		@type = @type.normalize_type(conf)
		if @default.nil?
			@default = @type.default_value
		end
		if @type.double_type? && @default.int?
			@default = FloatLiteral.new(@default.value.to_f)
		end
		if @type.bool_type? && @default.int?
			@default = BoolLiteral.new(@default.value != 0)
		end
	end

	# Gets ID number. Always Fixnum, not null.
	attr_accessor :id

	# Describes type. Type.
	attr_accessor :type

	# Gets name. FieldName.
	attr_accessor :field_name

	# Gets name. String.
	attr_reader :name

	# Gets default value. Literal.
	# If type of this field is ExternalType, it may be null.
	# Otherwise not null.
	attr_accessor :default

	# true if this field is declared as requred.
	def required?
		@required
	end

	# true if this field is declared as optional.
	def optional?
		!required?
	end
end


# Describes declaration of a `service`.
class Service < TypeDeclaration
	def initialize(name, extends, functions)
		@extends = extends.nil? ? nil : TypeName.new(extends)
		@functions = functions
		super(name)
	end

	# Gets base class. TypeName, nullable.
	attr_accessor :extends

	# Gets list of functions. Array of Function.
	attr_accessor :functions

	def normalize!(conf)  #:nodoc:
		@functions.each {|f| f.normalize!(conf) }
	end

	include TopLevelDeclaration
end

class Function
	def initialize(type, name, fields, exception_fields)  #:nodoc:
		@type = type
		@name = FunctionName.new(name)
		@function_name = FunctionName.new(name)
		@fields = fields
		@exception_fields = exception_fields
	end

	# Gets return type. Type.
	attr_accessor :type

	# Gets name. FunctionName.
	attr_accessor :function_name

	# Gets name. String.
	attr_reader :name

	# Gets arguments. FieldMap.
	attr_accessor :fields

	# Gets list of throwable exceptions. ExceptionFieldMap.
	attr_accessor :exception_fields

	def normalize!(conf)  #:nodoc:
		@exception_fields.normalize!(conf)
		@type = @type.normalize_type(conf)
		@fields.normalize!(conf)
	end
end

class ExceptionFieldMap < Hash
	def initialize(list)
		@list = list
		super()
	end

	include IDNormalizable  #:nodoc:

	def normalize!(conf)  #:nodoc:
		@list.each {|x| x.normalize!(conf) }
		@list.reject! {|f| f.type.void_type? }
		normalize_id(@list, self)
	end
end

class ExceptionField
	def initialize(id, type)  #:nodoc:
		@id = id
		@type = type
	end

	# Gets ID number. Fixnum.
	attr_accessor :id

	# Gets type. Type.
	attr_accessor :type

	def normalize!(conf)  #:nodoc:
		@type = @type.normalize_type(conf)
	end
end


class Type
	def initialize(name)  #:nodoc:
		@name = name
		@type_name = TypeName.new(name)
	end

	# Gets name. TypeName
	attr_accessor :type_name

	# Gets name. String.
	attr_reader :name

	# true if this is a BuiltInType.
	def builtin_type?
		false
	end

	def to_s
		@type_name.to_s
	end

	def integer_type?
		@name == 'int8' || @name == 'int16' || @name == 'int32' || @name == 'int64' ||
		@name == 'uint8' || @name == 'uint16' || @name == 'uint32' || @name == 'uint64'
	end

	def bool_type?
		@name == 'bool'
	end

	def double_type?
		@name == 'double'
	end

	def string_type?
		@name == 'string'
	end

	def bytes_type?
		@name == 'bytes'
	end

	def void_type?
		@name == 'void'
	end

	def list_type?
		@name == 'list'
	end

	def set_type?
		@name == 'set'
	end

	def map_type?
		@name == 'map'
	end

	def container_type?
		list_type? || set_type? || map_type?
	end

	def external_type?
		false
	end

	def base_type?
		!container_type? && !external_type?
	end
end

class BuiltInType < Type
	BUILTIN_NORMALIZE_MAP = {
		'byte'   => 'int8',
		'i8'     => 'int8',
		'i16'    => 'int16',
		'i32'    => 'int32',
		'i64'    => 'int64',
		'u8'     => 'uint8',
		'u16'    => 'uint16',
		'u32'    => 'uint32',
		'u64'    => 'uint64',
		'binary' => 'bytes',
	}

	def normalize!(conf)  #:nodoc:
		if n = BUILTIN_NORMALIZE_MAP[@name]
			@name = n
			@type_name = TypeName.new(@name)
		end
	end

	def normalize_type(conf)  #:nodoc:
		normalize!(conf)
		self
	end

	# true if this is a BuiltInType.
	def builtin_type?
		true
	end

	def default_value
		case @name
		when 'int8',  'int16',  'int32',  'int64'
			IntLiteral.new(0)
		when 'uint8', 'uint16', 'uint32', 'uint64'
			IntLiteral.new(0)
		when 'bool'
			BoolLiteral.new(false)
		when 'double'
			FloatLiteral.new(false)
		when 'bytes'
			BytesLiteral.new("")
		when 'string'
			StringLiteral.new("")
		when 'list'
			ListLiteral.new([])
		when 'set'
			ListLiteral.new([])
		when 'map'
			MapLiteral.new({})
		else
			VarLiteral.new(@type_name.to_s.to_sym)
		end
	end
end

class ListType < BuiltInType
	def initialize(element_type)  #:nodoc:
		super("list")
		@element_type = element_type
	end

	# Gets declared type of elements. Type.
	attr_accessor :element_type

	def normalize!(conf)  #:nodoc:
		super
		@element_type = @element_type.normalize_type(conf)
	end
end

class SetType < BuiltInType
	def initialize(element_type)  #:nodoc:
		super("set")
		@element_type = element_type
	end

	# Gets declared type of elements. Type.
	attr_accessor :element_type

	def normalize!(conf)  #:nodoc:
		super
		@element_type = @element_type.normalize_type(conf)
	end
end

class MapType < BuiltInType
	def initialize(key_type, value_type)  #:nodoc:
		super("map")
		@key_type = key_type
		@value_type = value_type
	end

	# Gets declared type of keys. Type.
	attr_accessor :key_type

	# Gets declared type of values. Type.
	attr_accessor :value_type

	def normalize!(conf)  #:nodoc:
		super
		@key_type = @key_type.normalize_type(conf)
		@value_type = @value_type.normalize_type(conf)
	end
end


class ExternalType < Type
	def external_type?
		true
	end

	def default_value
		nil
	end

	def normalize!(conf)
	end

	def normalize_type(conf)  #:nodoc:
		if type = conf[:typedef][@name]
			type
		else
			self
		end
	end
end


class Literal
	def initialize(value)
		@value = value
	end

	attr_accessor :value

	def to_s
		@value.to_s
	end

	def var?
		self.class == VarLiteral
	end

	def string?
		self.class == StringLiteral
	end

	def int?
		self.class == IntLiteral
	end

	def float?
		self.class == FloatLiteral
	end

	def bytes?
		self.class == BytesLiteral
	end

	def list?
		self.class == ListLiteral
	end

	def map?
		self.class == MapLiteral
	end

	def container?
		list? || map?
	end
end

class VarLiteral < Literal
end

class StringLiteral < Literal
	def to_s
		@value.dump
	end
end

class IntLiteral < Literal
end

class FloatLiteral < Literal
end

class BoolLiteral < Literal
	def to_s
		@value ? "true" : "false"
	end
end

class BytesLiteral < Literal
	def to_s
		@value.dump
	end
end

class ListLiteral < Literal
	# FIXME to_s
end

class MapLiteral < Literal
	# FIXME to_s
end


end  # module AST

