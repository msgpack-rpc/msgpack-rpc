%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)
%gen_guard("#{type_name}_server") do

#include "{{name}}.hpp"

%gen_package(doc) do

namespace {{type_name}} {


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
