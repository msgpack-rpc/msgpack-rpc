%guard = "MPRPC_#{name}_#{"%08x"%rand(1<<32)}_HPP__"
#ifndef {{guard}}
#define {{guard}}

#include "types.hpp"
%if nss = doc.namespace(:cpp)
namespace [%ns%] {  %|ns| nss.each
%end

namespace {{name}} {


class server : public msgpack::rpc::server::base {
public:
	server(msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::server::base(lo) { }

	~server() { }

	void dispatch(msgpack::rpc::request req);

public:
	%functions.each do |f|
	void {{f.name}}(msgpack::rpc::request::type<{{f.type}}>[%join(f.fields,''){|a|", #{a}"}%]);
	%end
};


class client : public msgpack::rpc::client::base {
public:
	client(const msgpack::rpc::address& addr, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(addr, lo) { }

	client(const std::string& host, uint16_t port, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(host, port, lo) { }

	~client() { }
	%functions.each do |f|

	{{f.type}} {{f.name}}([%join(f.fields){|a|"const #{a.type}& #{a.name}"}%])
		{ return instance.call("{{f.name}}"[%join(f.fields,''){|a|", #{a.name}"}%]).get<{{f.type}}>(); }

	msgpack::rpc::future::type<{{f.type}}> {{f.name}}_async([%join(f.fields){|a|"const #{a.type}& #{a.name}"}%])
		{ return instance.call("{{f.name}}"[%join(f.fields,''){|a|", #{a.name}"}%]); }
	%end
};


}  // namespace {{name}}

/*
%functions.each do |f|
void {{name}}::server::{{f.name}}(msgpack::rpc::request::type<{{f.type}}> req[%join(f.fields,''){|a|", #{a}"}%])
%end
*/

%if nss = doc.namespace(:cpp)
}  // namespace [%ns%]  %|ns| nss.reverse.each
%end

#endif
