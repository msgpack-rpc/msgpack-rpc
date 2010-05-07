require 'forwardable'

module AST

module Util
	def join(array, sep = ', ', &block)
		block ||= Proc.new {|a| a }
		array.map(&block).join(sep)
	end
end

class Document
	include Util
	def initialize(headers, defs)
		@headers = headers
		@defs = defs
	end
	attr_reader :headers, :defs

	extend Forwardable
	def_delegators :@defs, :consts, :typedefs, :enums, :senums, :structs, :exceptions, :services
	def_delegators :@headers, :cpp_includes, :namespace, :php_namespaces
end

module Headers
	include Util
	def self.new(array)
		array.extend(Headers)
	end
	def cpp_includes
		select {|x| x.class == CppInclude }
	end
	def namespace(scope)
		select {|x| x.class == Namespace &&
			(x.scope.to_s == scope.to_s || x.scope.to_s == "*") }.last
	end
	def php_namespaces
		select {|x| x.class == PHPNamespace }
	end
end

module Definitions
	include Util
	def self.new(array)
		array.extend(Definitions)
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
	def senums
		select {|x| x.class == Senum }
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
end

class Include
	include Util
	def initialize(path)
		@path = path
	end
	attr_reader :path
	# FIXME
end

class CppInclude
	include Util
	def initialize(name)
		@name = name
	end
	attr_reader :name
	def to_s
		name.to_s
	end
end

class Namespace
	include Util
	def initialize(scope, name)
		@scope = scope
		@name = name
	end
	attr_reader :scope, :name
	def to_s
		name.to_s
	end
	def to_a
		name.to_s.split('.')
	end
end

class PHPNamespace
	include Util
	def initialize(name)
		@name = name
	end
	attr_reader :name
end

class Const
	include Util
	def initialize(type, name, value)
		@type = type
		@name = name
		@value = value
	end
	attr_reader :type, :name, :value
end

class Typedef
	include Util
	def initialize(type, name)
		@type = type
		@name = name
	end
	attr_reader :type, :name
end

class EnumField
	include Util
	def initialize(name, value)
		@name = name
		@value = value
	end
	attr_reader :name, :value
end

class Enum
	include Util
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_reader :name, :fields
end

class SenumField
	include Util
	def initialize(name)
		@name = name
	end
	attr_reader :name
	def to_s
		@name.to_s
	end
end

class Senum
	include Util
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_reader :name, :fields
end

class Struct
	include Util
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_reader :name, :fields
end

class Exception < Struct
	def initialize(name, fields)
		@name = name
		@fields = fields
	end
	attr_reader :name, :fields
end

class Service
	include Util
	def initialize(name, extends, functions)
		@name = name
		@extends = extends
		@functions = functions
	end
	attr_reader :name, :extends, :functions
end

class Function
	include Util
	def initialize(type, name, fields, thorws)
		@type = type
		@name = name
		@fields = fields
		@throws = throws
	end
	attr_reader :type, :name, :fields, :throws
end

class Field
	include Util
	def initialize(id, required, type, name, default)
		@id = id
		@required = required
		@type = type
		@name = name
		@default = default
	end
	attr_reader :id, :required, :type, :name, :default
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
	def initialize(name, element, cpp_type)
		super(name)
		@element = element
		@cpp_type = cpp_type
	end
end

class SetType < Type
	def initialize(name, element, cpp_type)
		super(name)
		@element = element
		@cpp_type = cpp_type
	end
end

class MapType < Type
	def initialize(name, key, value, cpp_type)
		super(name)
		@key = key
		@value = value
		@cpp_type = cpp_type
	end
end

end

