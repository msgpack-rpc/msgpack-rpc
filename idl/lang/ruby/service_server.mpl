%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

require File.join(File.dirname(__FILE__), '{{name}}')

%gen_package(doc) do


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
