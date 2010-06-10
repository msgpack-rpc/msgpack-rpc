module AST


module Util
	def initialize_util
		@data = {}
	end

	def normalize!(conf)
	end
end

module IDNormalizable
	# Struct, Exception, ThrowsList
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


class Document < Array
	def initialize(defs)
		initialize_util
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
		initialize_util
		@name = name
	end
end


class Namespace < Array
	def initialize(scope, name)
		initialize_util
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
		initialize_util
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
		initialize_util
		@type = type
		@name = name
	end

	def normalize!(conf)
		@type.normalize!(conf)
	end
end


class Enum
	def initialize(name, fields)
		initialize_util
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
		initialize_util
		@name = name
		@value = value
	end
end


class Struct
	def initialize(name, fields)
		initialize_util
		@name = name
		@fields = fields
	end

	def normalize!(conf)
		@fields.normalize!(conf)
	end
end

class Exception < Struct
	def initialize(name, fields)
		super(name, fields)
	end
end

class FieldList < Hash
	def initialize(fields)
		initialize_util
		@list = fields
		super()
	end

	include IDNormalizable
	def normalize!(conf)
		@list.reject! {|f| f.type.void? }
		@min_id, @max_id = normalize_id(@list, self)
		each_value {|f| f.normalize!(conf) }
	end
end

class Field
	def initialize(id, qualifier, type, name, default)
		initialize_util
		@id = id
		@qualifier = qualifier
		@type = type
		@name = name
		@default = default
	end

	def normalize!(conf)
		if !@default.nil? || @qualifier == "required"
			@required = true
		else
			@required = false
		end
		@type.normalize!(conf)
		if @default.nil?
			@default = @type.default_value
		end
		if @type.double? && @default.int?
			@default = FloatValue.new(@default.value.to_f)
		end
		if @type.bool? && @default.int?
			@default = BoolValue.new(@default.value != 0)
		end
	end
end


class Service
	def initialize(name, extends, functions)
		initialize_util
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
		initialize_util
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


class ThrowsList < Hash
	def initialize(list)
		initialize_util
		@list = list
		super()
	end

	include IDNormalizable
	def normalize!(conf)
		normalize_id(@list, self)
	end
end

class ThrowsClass
	def initialize(id, name)
		initialize_util
		@id = id
		@name = name
	end
end


class Type
	def initialize(name)
		initialize_util
		@name = name
	end

	def default_value
		case @name
		when 'int8',  'int16',  'int32',  'int64'
			IntValue.new(0)
		when 'uint8', 'uint16', 'uint32', 'uint64'
			IntValue.new(0)
		when 'bool'
			BoolValue.new(false)
		when 'double'
			FloatValue.new(false)
		when 'bytes'
			BytesValue.new("")
		when 'string'
			StringValue.new("")
		when 'list'
			ListValue.new([])
		when 'set'
			ListValue.new([])
		when 'map'
			MapValue.new({})
		else
			ConstValue.new(@name.to_sym)
		end
	end

	TYPE_NORMALIZE_MAP = {
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
		initialize_util
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

class BytesValue < Value
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

