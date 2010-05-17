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

	def consts
		select {|x| x.class == Const }
	end

	def typedefs
		select {|x| x.class == Typedef }
	end

	def enums
		select {|x| x.class == Enum }
	end

	def structs
		select {|x| x.class == Struct }
	end

	def exceptions
		select {|x| x.class == Exception }
	end

	def services
		select {|x| x.class == Service }
	end

	def namespace(scope)
		select {|x| x.class == Namespace &&
			(x.scope.to_s == scope.to_s || x.scope.to_s == "*") }.last
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

	# fields is an Array of Field.
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

class Field
	include Util

	# id is a always Fixnum, not null.
	# qualifier is "required" or "optional", not null.
	# default is a Value, nullable.
	attr_accessor :id, :qualifier, :type, :name, :default

	def required?
		qualifier == "required"
	end

	def optional?
		qualifier == "optional"
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

	# throws is an ThrowsList.
	attr_accessor :type, :name, :fields, :throws
end


class ThrowsList < Array
	include Util
end

class ThrowsClass
	include Util
	attr_accessor :id, :name
end


class Type
	include Util
	def to_s
		@name.to_s
	end
end

class ListType < Type
end

class SetType < Type
end

class MapType < Type
end


class Value
	include Util
	attr_accessor :value

	def const?
		type == :const
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

