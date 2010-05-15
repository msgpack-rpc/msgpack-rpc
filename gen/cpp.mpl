__BEGIN__
class AST::Type
	@@typemap = {
		:bool   => 'bool',
		:byte   => 'int8_t',
		:i16    => 'int16_t',
		:i32    => 'int32_t',
		:i64    => 'int64_t',
		:double => 'double',
		:string => 'std::string',
		:binary => 'msgpack::type::raw_ref',
		:slist  => '',   # FIXME
		:list   => 'std::vector',
		:set    => 'std::set',
		:map    => 'std::map',
	}
	def to_s
		if map = @@typemap[@name]
			map
		else
			@name.to_s
		end
	end
end

module AST::Util
	def expand_template(*types)
		#"<#{types.join(',').gsub('>>','> >')}>"
		"<#{types.join(',')}> "
	end
end

class AST::ListType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@element)
	end
end

class AST::SetType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@element)
	end
end

class AST::MapType
	def to_s
		return @cpp_type.to_s if @cpp_type
		super+expand_template(@key, @value)
	end
end

class AST::Field
	def to_s
		"#{type} #{name}"
	end
end
__END__
#include <msgpack/rpc/server.h>
#include <msgpack/rpc/client.h>
#include <string>
#include <map>
#include <set>
#include <vector>
#include [%inc%]  %|inc| cpp_includes.each

%if nss = namespace(:cpp)
namespace [%ns%] {  %|ns| nss.to_a.each
%end


%defs.each do |d|
%case d
%when AST::Const
const [%d.type%] [%d.name%] = [%d.value%];  // FIXME

%when AST::Typedef
typedef [%d.type%] [%d.name%];

%when AST::Enum
enum [%d.name%] {
	%d.fields.each do |f|
	[%f.name%][%:if f.value then%] = [%f.value%][%:end%],
	%end
};

%when AST::Senum
%# FIXME

%when AST::Struct
struct [%d.name%] {
	%d.fields.each do |f|
	[%f.type%] [%f.name%];  // FIXME id, type, required, optional
	%end
	MSGPACK_DEFINE([%join(d.fields){|f|f.name}%]);
};

%when AST::Exception
// FIXME Exception [%d.name%]
%when AST::Service
	%# do nothing
%end
%end

%services.each do |srv|

namespace [%srv.name%] {

class server : public msgpack::rpc::server::base {
public:
	server(msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::server::base(lo) { }

	~server() { }

	void dispatch(msgpack::rpc::request req)
	try {
		std::string method;
		req.method().convert(&method);

		%srv.functions.each do |f|
		if(method == "[%f.name%]") {
			msgpack::type::tuple<[%join(f.fields){|a|"#{a.type}"}%]> args;
			req.params().convert(&args);
			[%f.name%](msgpack::rpc::request::type<[%f.type%]>(req)[%i=-1;join(f.fields,''){|a|", args.get<#{i+=1}>()"}%]);
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

public:
	%srv.functions.each do |f|
	void [%f.name%](msgpack::rpc::request::type<[%f.type%]>[%join(f.fields,''){|a|", #{a}"}%]);
	%end
};

class client : public msgpack::rpc::client::base {
public:
	client(const msgpack::rpc::address& addr, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(addr, lo) { }

	client(const std::string& host, uint16_t port, msgpack::rpc::loop lo = msgpack::rpc::loop()) :
		msgpack::rpc::client::base(host, port, lo) { }

	~client() { }
	%srv.functions.each do |f|

	[%f.type%] [%f.name%]([%join(f.fields){|a|"const #{a.type}& #{a.name}"}%])
		{ return instance.call("[%f.name%]"[%join(f.fields,''){|a|", #{a.name}"}%]).get<[%f.type%]>(); }

	msgpack::rpc::future::type<[%f.type%]> [%f.name%]_async([%join(f.fields){|a|"const #{a.type}& #{a.name}"}%])
		{ return instance.call("[%f.name%]"[%join(f.fields,''){|a|", #{a.name}"}%]); }
	%end
};

}  // namespace [%srv.name%]

/*
%srv.functions.each do |f|
void [%srv.name%]::server::[%f.name%](msgpack::rpc::request::type<[%f.type%]> req[%join(f.fields,''){|a|", #{a}"}%])
%end
*/

%end

%if nss = namespace(:cpp)
}  // namespace [%ns%]  %|ns| nss.to_a.reverse.each
%end
