module AST


module Util
	def join(array, sep = ', ', &block)
		block ||= Proc.new {|a| a }
		array.map(&block).join(sep)
	end
	attr_accessor :document
	alias doc document
end


class Document < Array
	include Util
	def initialize(defs)
		defs.compact!
		defs.each {|d| d.document = self }
		super(defs)
	end

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
	def initialize(name)
		@name = name
	end
	attr_accessor :name
	def to_s
		name.to_s
	end
end


class Namespace < Array
	include Util
	def initialize(scope, name)
		@scope = scope
		@name = name
		super(name.split('.'))
	end
	attr_accessor :scope, :name
	def to_s
		@name
	end
end


class Const
	include Util
	def initialize(type, name, value)
		@type = type
		@name = name
		@value = value
	end
	attr_accessor :type, :name, :value
end


class Typedef
	include Util
	def initialize(type, name)
		@type = type
		@name = name
	end
	attr_accessor :type, :name
end


class EnumField
	include Util
	def initialize(name, value)
		@name = name
		@value = value
	end
	attr_accessor :name, :value
end

class Enum
	include Util
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_accessor :name, :fields
end


class Struct
	include Util
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_accessor :name, :fields
end


class Exception < Struct
	def initialize(name, fields)
		super(name, fields)
	end
	attr_accessor :name
end


class Service
	include Util
	def initialize(name, extends, functions)
		@name = name
		@extends = extends
		@functions = functions
	end
	attr_accessor :name, :extends, :functions
end


class Function
	include Util
	def initialize(type, name, fields, thorws)
		@type = type
		@name = name
		@fields = fields
		@throws = throws
	end
	attr_accessor :type, :name, :fields, :throws
end


class Field
	include Util
	def initialize(id, qualifier, type, name, default)
		@id = id
		@qualifier = qualifier
		@type = type
		@name = name
		@default = default
	end
	attr_accessor :id, :qualifier, :type, :name, :default
	def required?
		@qualifier == "required"
	end
	def optional?
		@qualifier == "optional"
	end
end


class Type
	include Util
	def initialize(name)
		@name = name.to_sym
	end
	def to_s
		@name.to_s
	end
end


class ListType < Type
	def initialize(element)
		super("list")
		@element = element
	end
end


class SetType < Type
	def initialize(element)
		super("set")
		@element = element
	end
end


class MapType < Type
	def initialize(key, value)
		super("map")
		@key = key
		@value = value
	end
end


end

