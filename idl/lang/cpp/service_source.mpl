%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

#include "{{name}}_server.hpp"
#include <memory>
#include <mp/unordered_map.h>

%gen_package(doc) do

namespace {{name}} {


%functions.each do |m|
static void dispatch_{{m.function_name}}(server* svr, msgpack::rpc::request* preq)
{
	{{type_name}}::{{m.function_name}} message;
	preq->params().convert(&message);
	svr->{{m.function_name}}(*preq, message);
}
%end

typedef mp::unordered_map<std::string, void (*)(server*, msgpack::rpc::request*)> table_type;
#define TABLE server::s_dispatch_table.pimpl
server::dispatch_table server::s_dispatch_table;

server::dispatch_table::dispatch_table()
{
	std::auto_ptr<table_type> table(new table_type());
	%functions.each do |m|
	table->insert(std::make_pair("{{m.function_name}}", &dispatch_{{m.function_name}}));
	%end
	TABLE = (void*)table.release();
}

server::dispatch_table::~dispatch_table()
{
	delete (table_type*)TABLE;
}

void server::dispatch(msgpack::rpc::request req)
try {
	std::string method;
	req.method().convert(&method);

	const table_type* table((table_type*)TABLE);

	table_type::const_iterator m = table->find(method);
	if(m == table->end()) {
		req.error(msgpack::rpc::NO_METHOD_ERROR);
		return;
	}

	(*m->second)(this, &req);

} catch (msgpack::type_error& e) {
	req.error(msgpack::rpc::ARGUMENT_ERROR);
	return;

} catch (std::exception& e) {
	req.error(std::string(e.what()));
	return;
}


}  // namespace {{name}}

%end  # gen_package
