module AST


module Util
	def normalize!(conf)
	end
end

module IDNormalizable
	# Struct, Exception, ThrowsList
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

	SCOPE_NORMALIZE_MAP = {
		'rb'   => 'ruby',
		'py'   => 'python',
	}

	def normalize!(conf)
		if n = SCOPE_NORMALIZE_MAP[@scope]
			@scope = n
		end
	end
end


class Const
	def initialize(type, name, value)
		@type = type
		@name = name
		@value = value
	end

	def normalize!(conf)
		@type.normalize!(conf)
	end
end


class Typedef
	def initialize(type, name)
		@type = type
		@name = name
	end

	def normalize!(conf)
		@type.normalize!(conf)
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
		@type.normalize!(conf)
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
		@type.normalize!(conf)
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
		@name = name
	end

	TYPE_NORMALIZE_MAP = {
		'byte' => 'int8',
		'i8'   => 'int8',
		'i16'  => 'int16',
		'i32'  => 'int32',
		'i64'  => 'int64',
		'u8'   => 'uint8',
		'u16'  => 'uint16',
		'u32'  => 'uint32',
		'u64'  => 'uint64',
	}

	def normalize!(conf)
		if n = TYPE_NORMALIZE_MAP[@name]
			@name = n
		end
	end
end

class ListType < Type
	def initialize(element_type)
		super("list")
		@element_type = element_type
	end

	def normalize!(conf)
		super
		@element_type.normalize!(conf)
	end
end

class SetType < Type
	def initialize(element_type)
		super("set")
		@element_type = element_type
	end

	def normalize!(conf)
		super
		@element_type.normalize!(conf)
	end
end

class MapType < Type
	def initialize(key_type, value_type)
		super("map")
		@key_type = key_type
		@value_type = value_type
	end

	def normalize!(conf)
		super
		@key_type.normalize!(conf)
		@value_type.normalize!(conf)
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

