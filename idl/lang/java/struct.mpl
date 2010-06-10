%nss = doc.namespace(:java)
package {{nss.join('.')}}; %>unless nss.empty?

import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.io.IOException;
import org.msgpack.Packer;
import org.msgpack.Unpacker;
import org.msgpack.MessagePackable;
import org.msgpack.MessageUnpackable;
import org.msgpack.MessageConvertable;
import org.msgpack.MessageTypeException;
import org.msgpack.Schema;
import org.msgpack.schema.*;

%$anon_seqid = 0
%def next_anon
%	"_A#{$anon_seqid+=1}"
%end

%def default_field(f)
	%gen_literal(f.type, f.default, "this.#{f.name}")
%end

%def gen_literal(type, val, name = nil)
	%if name
		%decl = "#{name}"
	%else
		%name = next_anon
		%decl = "#{type} #{name}"
	%end
	%if type.bytes?
		{{decl}} = new byte[{{val.value.length}}];
	%elsif type.string?
		{{decl}} = {{val.value.dump}};
	%elsif type.base_type?
		{{decl}} = {{val.value}};
	%elsif type.list?
		{{decl}} = new ArrayList();
		%val.value.each {|e|
			%ename = gen_literal(type.element_type, e)
			{{name}}.add({{ename}});
		%}
	%elsif type.set?
		{{decl}} = new HashSet();
		%val.value.each {|e|
			%ename = gen_literal(type.element_type, e)
			{{name}}.add({{ename}});
		%}
	%elsif type.map?
		{{decl}} = new HashMap();
		%val.value.each_pair {|k,v|
			kname = gen_literal(type.key_type, k)
			vname = gen_literal(type.value_type, v)
			{{name}}.put({{kname}}, {{vname}});
		%}
	%else
		{{decl}} = new {{val.value}}();
	%end
%end

%def unpack_field(f)
	%gen_unpack(f.type, "this.#{f.name}")
%end

%def gen_unpack(type, name = nil)
	%if name.nil?
		%name = next_anon
		%decl = "#{type} #{name}"
	%else
		%decl = "#{name}"
	%end
	%if type.base_type?
		%case type.name
		%when 'int8'
			{{decl}} = _Pac.unpackByte();
		%when 'int16'
			{{decl}} = _Pac.unpackShort();
		%when 'int32'
			{{decl}} = _Pac.unpackInt();
		%when 'int64'
			{{decl}} = _Pac.unpackLong();
		%when 'uint8'
			{{decl}} = _Pac.unpackByte();
		%when 'uint16'
			{{decl}} = _Pac.unpackShort();
		%when 'uint32'
			{{decl}} = _Pac.unpackInt();
		%when 'uint64'
			{{decl}} = _Pac.unpackLong();
		%when 'double'
			{{decl}} = _Pac.unpackDouble();
		%when 'bool'
			{{decl}} = _Pac.unpackBoolean();
		%when 'bytes'
			{{decl}} = _Pac.unpackByteArray();
		%when 'string'
			{{decl}} = _Pac.unpackString();
		%end
	%elsif type.user_type?
		{{decl}} = new {{type}}();
		{{name}}.messageUnpack(_Pac);
	%elsif type.list? || type.set?
		%length = next_anon
		%element_type = type.element_type
		int {{length}} = _Pac.unpackArray();
		{{decl}} = new ArrayList({{length}});  %>if type.list?
		{{decl}} = new HashSet({{length}});    %>if type.set?
		%i = next_anon
		for(int {{i}}=0; {{i}} < {{length}}; {{i}}++) {
			%vname = gen_unpack(element_type)
			{{name}}.add({{vname}});
		}
	%else
		%length = next_anon
		%key_type = type.key_type
		%value_type = type.value_type
		int {{length}} = _Pac.unpackArray();
		{{decl}} = new HashMap({{length}});
		%i = next_anon
		for(int {{i}}=0; {{i}} < {{length}}; {{i}}++) {
			%kname = gen_unpack(key_type)
			%vname = gen_unpack(value_type)
			{{name}}.put({{kname}}, {{vname}});
		}
	%end
	%return name
%end


public class {{name}} implements MessagePackable, MessageUnpackable, MessageConvertable {
	%fields.each_value do |f|
	public {{f.type}} {{f.name}};
	%end

	public static {{name}} createDefault() {
		{{name}} obj = new {{name}}();
		obj._Default();
		return obj;
	}

	public static {{name}} unpack(Unpacker _Pac) throws IOException {
		{{name}} obj = new {{name}}();
		obj.messageUnpack(_Pac);
		return obj;
	}

	public static {{name}} convert(Object deserialized) {
		{{name}} obj = new {{name}}();
		obj.messageConvert(deserialized);
		return obj;
	}

	private void _Default() {
		%fields.each_value do |f|
			%default_field(f)
		%end
	}

	public void messagePack(Packer _Pk) throws IOException {
		_Pk.packArray({{fields.max_id}});
		%1.upto(fields.max_id) do |i|
		%if f = fields[i]
		_Pk.pack({{f.name}});
		%else
		_Pk.packNil();
		%end
		%end
	}

	public void messageUnpack(Unpacker _Pac) throws IOException, MessageTypeException {
		int _Length = _Pac.unpackArray();
		%max_required_id = fields.max_required_id
		%if max_required_id > 0
		if(_Length < {{max_required_id}}) {
			throw new MessageTypeException();
		}
		%end
		%1.upto(fields.max_id) do |i|
			%f = fields[i]
			%unless f
				_Pac.unpackObject();
				%next
			%end
			%if f.required?
				%unpack_field(f)
			%elsif i <= max_required_id
				if(_Pac.tryUnpackNull()) {
					%default_field(f)
				} else {
					%unpack_field(f)
				}
			%else
				if(_Length < {{i}}) {
					%default_field(f)
				} else {
					%unpack_field(f)
				}
			%end
		%end
		for(int i={{fields.max_id}}; i < _Length; i++) {
			_Pac.unpackObject();
		}
	}

	public void messageConvert(Object _Obj) throws MessageTypeException {
		Map<Integer,Object> _Fields;
		if(_Obj instanceof Map) {
			_Fields = (Map<Integer,Object>)_Obj;
		} else if(_Obj instanceof List) {
			// FIXME wrap instead of copy
			List list = (List)_Obj;
			_Fields = new HashMap(list.size());
			int i = 1;
			for(Object obj : list) {
				_Fields.put(i++, obj);
			}
		} else {
			throw new MessageTypeException();
		}
		%fields.each_pair do |i,f|
			%if f.required?
			{{f.type.convert_schema(f, "_Fields.get(#{i})")}}
			%else
			%name = next_anon
			Object {{name}} = _Fields.get({{i}});
			if({{name}} == null) {
				%default_field(f)
			} else {
				{{f.type.convert_schema(f, name)}}
			}
			%end
		%end
	}
}

