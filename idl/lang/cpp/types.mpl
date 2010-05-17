%guard = "MPRPC_TYPES_#{"%08x"%rand(1<<32)}_HPP__"
#ifndef {{guard}}
#define {{guard}}

#include <msgpack.hpp>

%if nss = namespace(:cpp)
namespace [%ns%] {  %|ns| nss.each
%end


%each do |d|
%case d
%when AST::Const
static const {{d.type}} {{d.name}} = {{d.value}};
%# FIXME list, map

%when AST::Typedef
typedef {{d.type}} {{d.name}};

%when AST::Enum
enum {{d.name}} {
	%d.fields.each do |f|
	{{f.name}} = {{f.value}},
	%end
};

%when AST::Struct
struct {{d.name}} {
	{{d.name}}() { }
	%d.fields.each do |f|
	{{f.type}} {{f.name}};
	%end
	MSGPACK_DEFINE([%join(d.fields){|f|f.name}%]);
	%#FIXME id, required, optional
};

%when AST::Exception
struct {{d.name}} : public std::exception {
	{{d.name}}() : std::exception("{{d.name}}") { }
	%d.fields.each do |f|
	{{f.type}} {{f.name}};
	%end
	MSGPACK_DEFINE([%join(d.fields){|f|f.name}%]);
	%#FIXME id, required, optional
};

%when AST::Service
	%# do nothing
%end
%end


%if nss = namespace(:cpp)
}  // namespace [%ns%]  %|ns| nss.reverse.each
%end

#endif
