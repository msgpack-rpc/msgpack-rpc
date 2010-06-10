module AST


module Util
	def join(array=self, sep = ', ', &block)
		block ||= Proc.new {|a| a }
		array.map(&block).join(sep)
	end
end


class Document < Array
	include Util

	attr_accessor :conf
	attr_accessor :data

	def consts
		Array[ select {|x| x.class == Const } ]
	end

	def typedefs
		Array[ select {|x| x.class == Typedef } ]
	end

	def enums
		Array[ select {|x| x.class == Enum } ]
	end

	def structs
		Array[ select {|x| x.class == Struct } ]
	end

	def exceptions
		Array[ select {|x| x.class == Exception } ]
	end

	def services
		Array[ select {|x| x.class == Service } ]
	end

	def namespace(scope)
		Array[
			select {|x| x.class == Namespace &&
				(x.scope.to_s == scope.to_s || x.scope.to_s == "*") }.last
		].compact
	end

	def cpp_includes
		select {|x| x.class == CppInclude }
	end
end


class CppInclude
	include Util

	attr_accessor :name

	def to_s
		name.to_s
	end

	attr_accessor :document
	alias doc document
end


# Namespace is an Array of domain name.
# e.g. org.msgpack.ns => ["org", "msgpack", "ns"]
class Namespace < Array
	include Util

	attr_accessor :scope, :name

	def to_s
		name.to_s
	end

	attr_accessor :document
	alias doc document
end


class Const
	include Util

	# value is always a Value, not null.
	attr_accessor :type, :name, :value

	attr_accessor :document
	alias doc document
end


class Typedef
	include Util
	attr_accessor :type, :name

	attr_accessor :document
	alias doc document
end


class Enum
	include Util

	# fields is an Array of EnumField.
	attr_accessor :name, :fields

	attr_accessor :document
	alias doc document
end

class EnumField
	include Util

	# value is always a Integer, not null.
	attr_accessor :name, :value
end


class Struct
	include Util

	# fields is an FieldList.
	attr_accessor :name, :fields

	def exception?
		false
	end

	attr_accessor :document
	alias doc document
end

# Exception is a Struct.
class Exception < Struct
	attr_accessor :name
	def exception?
		true
	end
end

# FieldList is a map of Field
class FieldList < Hash
	include Util

	def optionals
		Hash[ select {|i,f| f.required? } ]
	end

	def requireds
		Hash[ select {|i,f| f.required? } ]
	end

	attr_reader :min_id, :max_id

	def min_required_id
		required_id(:first)
	end

	def max_required_id
		required_id(:last)
	end

	private
	def required_id(method)
		array = select {|i,f| f.required? }
		return 0 if array.empty?
		array.send(method)[0]
	end
end

class Field
	include Util

	# id is a always Fixnum, not null.
	# type is a Type, describes the type of this field.
	# name is a String, describes the name of this field.
	# default describes the default value, not null
	attr_accessor :id, :type, :name, :default

	def required?
		@required
	end

	def optional?
		!required?
	end
end


class Service
	include Util

	# extends is a Symbol, nullable.
	# functions is an Array of Function.
	attr_accessor :name, :extends, :functions

	attr_accessor :document
	alias doc document
end


class Function
	include Util

	# fields is an FieldList.
	# throws is an ThrowsList.
	attr_accessor :type, :name, :fields, :throws
end


# ThrowsList is a map of ThrowsClass
class ThrowsList < Hash
	include Util
end

class ThrowsClass
	include Util
	attr_accessor :id, :name
end


class Type
	include Util

	attr_accessor :name

	def integer?
		name == 'int8' || name == 'int16' || name == 'int32' || name == 'int64' ||
		name == 'uint8' || name == 'uint16' || name == 'uint32' || name == 'uint64'
	end

	def bool?
		name == 'bool'
	end

	def double?
		name == 'double'
	end

	def string?
		name == 'string'
	end

	def bytes?
		name == 'bytes'
	end

	def list?
		false
	end

	def set?
		false
	end

	def map?
		false
	end

	def void?
		@name == 'void'
	end

	def base_type?
		TYPE_BASE_TYPES.include?(@name)
	end

	def container_type?
		list? || set? || map?
	end

	def builtin_type?
		container_type? || base_type?
	end

	def user_type?
		!builtin_type?
	end

	TYPE_BASE_TYPES = [
		'int8',
		'int16',
		'int32',
		'int64',
		'uint8',
		'uint16',
		'uint32',
		'uint64',
		'bool',
		'double',
		'bytes',
		'string',
		'void',
	]

	## base type:
	# int8
	# int16
	# int32
	# int64
	# uint8
	# uint16
	# uint32
	# uint64
	# bool
	# double  # 64bit double
	# bytes   # raw bytes
	# string  # UTF-8 string
	# void    # only for return type of Function

	## container type:
	# list<TYPE>  # ordered sequence
	# set<TYPE>   # unordered sequence
	# map<TYPE>   # unordered map

	## other:
	# typedef or user-defined type
end

class ListType < Type
	def list?
		true
	end
	attr_accessor :element_type
end

class SetType < Type
	def set?
		true
	end
	attr_accessor :element_type
end

class MapType < Type
	def map?
		true
	end
	attr_accessor :key_type, :value_type
end


class Value
	include Util
	attr_accessor :value

	def const?
		type == :const
	end

	def bytes?
		type == :bytes
	end

	def string?
		type == :string
	end

	def int?
		type == :int
	end

	def float?
		type == :float
	end

	def list?
		type == :list
	end

	def map?
		type == :map
	end

	def container?
		list? || map?
	end
end

class ConstValue < Value
	def type
		:const
	end
	alias name value
end

class StringValue < Value
	def type
		:string
	end
end

class IntValue < Value
	def type
		:int
	end
end

class FloatValue < Value
	def type
		:float
	end
end

class BoolValue < Value
	def type
		:bool
	end
end

class BytesValue < Value
	def type
		:bytes
	end
end

class ListValue < Value
	def type
		:list
	end
end

class MapValue < Value
	def type
		:map
	end
end


end

