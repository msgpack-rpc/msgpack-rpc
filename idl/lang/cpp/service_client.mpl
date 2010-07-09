%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)
%gen_guard("#{type_name}_client") do

#include "{{name}}.hpp"

%gen_package(doc) do

namespace {{type_name}} {


class client : public msgpack::rpc::client::base {
public:
	client(const msgpack::rpc::address& addr, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(addr, lo) { }

	client(const std::string& host, uint16_t port, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(host, port, lo) { }

	~client() { }

	%functions.each do |m|
	{{m.type}} {{m.function_name}}_apply(
			const {{type_name}}::{{m.function_name}}& message) {
		%if m.type.void_type?
		instance.call_apply("{{m.function_name}}", message).get<void>();
		%else
		return instance.call_apply("{{m.function_name}}", message).get<{{m.type}}>();
		%end
	}

	{{m.type}} {{m.function_name}}(
			[%xjoin(m.fields){|a|"const #{a.type}& #{a.field_name}"}%]) {
		{{type_name}}::{{m.function_name}} _Message;
		%m.fields.each_value do |f|
		_Message.{{f.field_name}} = {{f.field_name}};
		%end
		return {{m.function_name}}_apply(_Message);
	}

	msgpack::rpc::future::type<{{m.type}}> {{m.function_name}}_async_apply(
			const {{type_name}}::{{m.function_name}}& message) {
		return instance.call_apply("{{m.function_name}}", message);
	}

	msgpack::rpc::future::type<{{m.type}}> {{m.function_name}}_async(
			[%xjoin(m.fields){|a|"const #{a.type}& #{a.field_name}"}%]) {
		{{type_name}}::{{m.function_name}} _Message;
		%m.fields.each_value do |f|
		_Message.{{f.field_name}} = {{f.field_name}};
		%end
		return {{m.function_name}}_async_apply(_Message);
	}
	%end
};


}  // namespace {{name}}


%end  # gen_package
%end  # gen_guard
