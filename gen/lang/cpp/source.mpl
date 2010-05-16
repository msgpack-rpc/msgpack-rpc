#include "{{name}}.hpp"
%if nss = doc.namespace(:cpp)
namespace [%ns%] {  %|ns| nss.each
%end

namespace {{name}} {


void server::dispatch(msgpack::rpc::request req)
try {
	std::string method;
	req.method().convert(&method);

	%functions.each do |f|
	if(method == "{{f.name}}") {
		msgpack::type::tuple<[%join(f.fields){|a|"#{a.type}"}%]> args;
		req.params().convert(&args);
		{{f.name}}(msgpack::rpc::request::type<{{f.type}}>(req)[%i=-1;join(f.fields,''){|a|", args.get<#{i+=1}>()"}%]);
	} else
	%end
	{
		req.error(msgpack::rpc::NO_METHOD_ERROR);
		return;
	}

} catch (msgpack::type_error& e) {
	req.error(msgpack::rpc::ARGUMENT_ERROR);
	return;

} catch (std::exception& e) {
	req.error(std::string(e.what()));
	return;
}


}  // namespace {{name}}

%if nss = doc.namespace(:cpp)
}  // namespace [%ns%]  %|ns| nss.reverse.each
%end
