%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

require File.join File.dirname(__FILE__), 'types'

%gen_package(doc) do

%functions.each do |m|
class {{modname(m.function_name)}}
	%gen_struct(m.function_name, m.fields)
end
%end


class Client < MessagePack::RPC::Client::Base
	%functions.each do |m|
	def {{m.function_name}}_apply(message)
		@base.call_apply(:{{m.function_name}}, message)
	end

	def {{m.function_name}}([%xjoin(m.fields){|a|"#{a.field_name}"}%])
		_Message = {{type_name}}::{{modname(m.function_name)}}.new
		%m.fields.each_value do |f|
		_Message.{{f.field_name}} = {{f.field_name}}
		%end
		{{m.function_name}}_apply(_Message)
	end

	def {{m.function_name}}_async_apply(message)
		@base.call_async_apply(:{{m.function_name}}, message)
	end

	def {{m.function_name}}_async([%xjoin(m.fields){|a|"#{a.field_name}"}%])
		_Message = {{type_name}}::{{modname(m.function_name)}}.new
		%m.fields.each_value do |f|
		_Message.{{f.field_name}} = {{f.field_name}}
		%end
		{{m.function_name}}_async_apply(_Message)
	end
	%end
end


class Server < MessagePack::RPC::Server::Base
	# OVERRIDE THESE METHODS.
	%functions.each do |m|
	#
	#def {{m.function_name}}([%xjoin(m.fields){|a|"#{a.field_name}"}%])
	%# FIXME raise no method error, with "not implemented" message
	#end
	%end

	include MessagePack::RPC::Dispatcher

	def dispatch(method, param, &block)
		if m = DISPATCH_TABLE[method.to_s.to_sym]
			m.call(self, req, param, &block)
		else
			raise NoMethodError.new("undefined method `#{req.method}'")
		end
	end

	private
	%functions.each do |m|
	def self.dispatch_{{m.function_name}}(svr, req, param)
		msg = {{type_name}}::{{modname(m.function_name)}}.from_msgpack(param)
		svr.{{m.function_name}}([%xjoin(m.fields){|a|"msg.#{a.field_name}"}%])
	end
	%end

	DISPATCH_TABLE = {
		%functions.each do |m|
		:{{m.function_name}} => method(:dispatch_{{m.function_name}}),
		%end
	}
end


%end  # gen_package
