%doc = self
%Mplex.file(doc.data[:common_mpl], self)
%gen_guard("TYPES") do

#include <msgpack/rpc/client.h>
#include <msgpack/rpc/server.h>
#include <stdexcept>

%gen_package(doc) do


%doc.each do |d|
%case d
%when AST::Constant
static const {{d.type}} {{d.const_name}} = {{d.value}};
%# FIXME MapLiteral, ListLiteral

%when AST::Enum
enum {{d.type_name}}_enum {
	%d.enum.each do |e|
	{{e.field_name}} = {{e.num}},
	%end
};

struct {{d.type_name}} {
	typedef {{d.type_name}}_enum type;

	{{d.type_name}}() : value({{d.enum.first.field_name}}) { }
	{{d.type_name}}(type v) : value(v) { }

	type value;

	template <typename Packer>
	void msgpack_pack(Packer& pk) const {
		pk.pack((int)value);
	}

	void msgpack_unpack(msgpack::object obj) {
		int v = obj.as<int>();
		switch(v) {
		%d.enum.each do |e|
		case {{e.field_name}}:
		%end
			value = (type)v;
			break;
		default:
			throw msgpack::type_error();
		}
	}
};

%when AST::Exception
struct {{d.name}} {
	%#FIXME : msgpack::rpc::remote_error
	%gen_struct(d.type_name, d.fields)
};

%when AST::Struct
struct {{d.type_name}} {
	%gen_struct(d.type_name, d.fields)
};

%when AST::Service
	%# done in header.mpl and source.mpl

%end  # case
%end  # doc.each


%end  # gen_package
%end  # gen_guard
