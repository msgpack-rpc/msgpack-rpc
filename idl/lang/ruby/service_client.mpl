%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

require File.join(File.dirname(__FILE__), '{{name}}')

%gen_package(doc) do


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


%end  # gen_package
