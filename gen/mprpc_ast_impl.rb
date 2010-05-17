module AST


module Util
	def normalize!(conf)
	end
end

module IDNormalizable
	def normalize_id(fields)
		used = []
		fields.each do |f|
			if id = f.id
				used<< id
			end
		end

		n = 1
		fields.each do |f|
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
	end
end


class Document < Array
	def initialize(defs)
		super(defs)
	end

	def normalize!(conf)
		@conf = conf
		compact!
		each {|d| d.document = self }
		each {|d| d.normalize!(conf) }
	end
end


class CppInclude
	def initialize(name)
		@name = name
	end
end


class Namespace < Array
	def initialize(scope, name)
		@scope = scope
		@name = name
		super(name.split('.'))
	end
end


class Const
	def initialize(type, name, value)
		@type = type
		@name = name
		@value = value
	end
end


class Typedef
	def initialize(type, name)
		@type = type
		@name = name
	end
end


class Enum
	def initialize(name, fields)
		@name = name
		@fields = fields
	end

	def normalize!(conf)
		n = 0
		@fields.each do |f|
			if val = f.value
				n = val+1
			else
				f.value = n
				n += 1
			end
		end
	end
end

class EnumField
	def initialize(name, value)
		@name = name
		@value = value
	end
end


class Struct
	def initialize(name, fields)
		@name = name
		@fields = fields
	end

	include IDNormalizable
	def normalize!(conf)
		normalize_id(@fields)
		@fields.each {|f| f.normalize!(conf) }
	end
end

class Exception < Struct
	def initialize(name, fields)
		super(name, fields)
	end
end

class Field
	def initialize(id, qualifier, type, name, default)
		@id = id
		@qualifier = qualifier
		@type = type
		@name = name
		@default = default
	end

	def normalize!(conf)
		unless @qualifier
			@qualifier = "required"
		end
	end
end


class Service
	def initialize(name, extends, functions)
		@name = name
		@extends = extends
		@functions = functions
	end

	def normalize!(conf)
		@functions.each {|f| f.normalize!(conf) }
	end
end


class Function
	def initialize(type, name, fields, throws)
		@type = type
		@name = name
		@fields = fields
		@throws = throws
	end

	def normalize!(conf)
		@throws.normalize!(conf)
	end
end


class ThrowsList
	def initialize(list)
		super(list)
	end

	include IDNormalizable
	def normalize!(conf)
		normalize_id(self)
	end
end

class ThrowsClass
	def initialize(id, name)
		@id = id
		@name = name
	end
end


class Type
	def initialize(name)
		@name = name.to_sym
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


class Value
	def initialize(value)
		@value = value
	end
end

class ConstValue < Value
	def initialize(value)
		super(value)
	end
end

class StringValue < Value
	def initialize(value)
		super(value)
	end
end

class IntValue < Value
	def initialize(value)
		super(value)
	end
end

class FloatValue < Value
	def initialize(value)
		super(value)
	end
end

class BoolValue < Value
	def initialize(value)
		super(value)
	end
end

class ListValue < Value
	def initialize(value)
		super(value)
	end
end

class MapValue < Value
	def initialize(value)
		super(value)
	end
end


end

