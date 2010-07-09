%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)
%gen_guard(type_name) do

#include "types.hpp"

%gen_package(doc) do

namespace {{type_name}} {


%functions.each do |m|
struct {{m.function_name}} {
	%gen_struct(m.function_name, m.fields)
};
%end

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


class server : public msgpack::rpc::server::base {
public:
	server(msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::server::base(lo) { }

	~server() { }

	void dispatch(msgpack::rpc::request req);

private:
	class dispatch_table {
	public:
		dispatch_table();
		~dispatch_table();
		void* pimpl;
	};

	static dispatch_table s_dispatch_table;
	friend class dispatch_table;

public:
	%functions.each do |m|
	void {{m.function_name}}(msgpack::rpc::request::type<{{m.type}}>, {{type_name}}::{{m.function_name}}&);
	%end
};


}  // namespace {{name}}


/*
%functions.each do |m|
void {{type_name}}::server::{{m.function_name}}(msgpack::rpc::request::type<{{m.type}}> req, {{type_name}}::{{m.function_name}}& params)
%end
*/


%end  # gen_package
%end  # gen_guard
