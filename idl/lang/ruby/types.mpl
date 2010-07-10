%doc = self
%Mplex.file(doc.data[:common_mpl], self)

require 'msgpack/rpc'

%gen_package(doc) do


%each do |d|
%case d
%when AST::Constant
{{d.const_name}} = {{d.value}}

%when AST::Enum
%d.enum.each do |e|
{{e.field_name}} = {{e.num}}
%end

class {{d.type_name}}
	def initialize(value = {{d.enum.first.field_name}})
		@value = value
	end
	attr_accessor :value

	def to_msgpack(out = '')
		value.to_msgpack(out)
	end

	def self.from_msgpack(obj)
		case obj
		when [%xjoin(d.enum){|x|x.num}%]
			@value = obj
		else
			raise "type error"
		end
	end
end

%when AST::Exception
class {{d.type_name}} < MessagePack::RPC::RemoteError
	%gen_struct(d.type_name, d.fields)
	%#FIXME
end

%when AST::Struct
class {{d.type_name}}
	%gen_struct(d.type_name, d.fields)
end

%when AST::Service
	%# done in client.mpl and server.mpl
%end
%end


%end  # gen_package
