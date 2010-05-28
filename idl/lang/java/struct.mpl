%if nss = doc.namespace(:java)
package {{nss.join('.')}};
%end

import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.io.IOException;
import org.msgpack.Packer;
import org.msgpack.Unacker;
import org.msgpack.MessagePackable;
import org.msgpack.MessageUnackable;
import org.msgpack.MessageConvertable;
import org.msgpack.MessageTypeException;

%$anon_seqid = 0
%def next_anon
%	"_A#{$anon_seqid+=1}"
%end

%def expand_literal(typename, val, name = nil)
%unless name
	%name = next_anon
	%anon = true
%end
%if !val.container?
	%if anon
		{{typename}} {{name}} = {{val}};
	%else
		{{name}} = {{val}};
	%end
%else
	%if anon
		{{typename}} {{name}} = new {{typename}}();
	%else
		{{name}} = new {{typename}}();
	%end
	%if val.list?
		%val.value.each {|v|
%vname = expand_literal(v)
		{{name}}.add({{vname}});
		%}
	%else  # map
		%val.value.each_pair {|k,v|
%kname = expand_literal(k.typename, k)
%vname = expand_literal(v.typename, v)
		{{name}}.put({{kname}}, {{vname}});
		%}
	%end
%end
%return name
%end  # def expand_literal

%def unpack_builtin_type(type)
%case type.name
%when 'int8';   '_Pac.unpackByte()'
%when 'int16';  '_Pac.unpackShort()'
%when 'int32';  '_Pac.unpackInt()'
%when 'int64';  '_Pac.unpacklong()'
%when 'uint8';  '_Pac.unpackByte()'
%when 'uint16'; '_Pac.unpackShort()'
%when 'uint32'; '_Pac.unpackInt()'
%when 'uint64'; '_Pac.unpackLong()'
%when 'bool';   '_Pac.unpackBoolean()'
%when 'bytes';  '_Pac.unpackByteArray()'
%when 'string'; '_Pac.unpackString()'
%end
%end

%def expand_unpack_container(type)
%length = next_anon
%name   = next_anon
%if type.list? || type.set?
	%element_type = type.element_type
	int {{length}} = _Pac.unpackArray();
	List {{name}} = new ArrayList({{length}});  %>if type.list?
	Set {{name}} = new HashSet({{length}});     %>if type.set?
	for(int _I=0; _I < {{length}}; _I++) {
		%if element_type.base_type?
			{{name}}.add({{unpack_builtin_type(element_type)}});
		%elsif element_type.container_type?
			%vname = expand_unpack_container(element_type)
			{{name}}.add({{vname}});
		%else
			%vname = next_anon
			{{element_type}} {{vname}} = new {{element_type}}();
			_Pac.unpack({{vname}});
			{{name}}.add({{vname}});
		%end
	}
%elsif type.map?
	%key_type = type.key_type
	%value_type = type.value_type
	int {{length}} = _Pac.unpackArray();
	Map {{name}} = new HashMap({{length}});
	for(int _I=0; _I < {{length}}; _I++) {
		%kname, vname = [key_type, value_type].map {|t|
		%if t.base_type?
			%n = next_anon
			{{t}} {{n}} = {{unpack_builtin_type(t)}};
		%elsif t.container_type?
			%n = expand_unpack_container(t)
		%else
			%n = next_anon
			{{t}} {{n}} = new {{t}}();
			_Pac.unpack({{n}});
		%end
		%n}
		{{name}}.put({{kname}}, {{vname}});
	}
%end
%return name
%end

%def unpack_field(f)
	%if f.type.base_type?
		{{f.name}} = {{unpack_builtin_type(f.type)}};
	%elsif f.type.container_type?
		%anon = expand_unpack_container(f.type);
		{{f.name}} = {{anon}};
	%else
		{{f.name}} = new {{f.type}}();
		_Pac.unpack({{f.name}});
	%end
%end

%def default_field(f)
	%if f.default
	%expand_literal(f.type.to_s, f.default, "this.#{f.name}")
	%elsif f.type.integer?
	%#this.{{f.name}} = 0;
	%elsif f.type.double?
	%#this.{{f.name}} = 0.0;
	%elsif f.type.bool?
	%#this.{{f.name}} = true;
	%elsif f.type.bytes?
	this.{{f.name}} = new byte[0];
	%else
	this.{{f.name}} = new {{f.type.to_s}}();
	%end
%end

class {{name}} implements MessagePackable, MessageUnackable, MessageConvertable {
	%fields.each do |f|
	public {{f.type}} {{f.name}};
	%end

	private void _Default() {
	%fields.each do |f|
		%default_field(f)
	%end
	}

	public void messagePack(Packer _Pk) throws IOException {
		%max_id = fields.max_id || 0
		_Pk.packArray({{max_id}});
		%1.upto(max_id) do |i|
			%if f = fields.get_id(i)
		_Pk.pack({{f.name}});
			%else
		_Pk.packNil();
			%end
		%end
	}

	public void messageUnpack(Unpacker _Pac) throws IOException, MessageTypeException {
		int _Length = _Pac.unpackArray();
	%max_id = fields.max_id || 0
	%max_required_id = fields.max_required_id || 0
		if(_Length < {{max_required_id}}) { throw new MessageTypeException(); }   %>if max_required_id > 0
	%n = 0
	%1.upto(max_required_id) do |i|
		%if f = fields.get_id(i)
			%if f.optional?
	if(!_Pk.tryUnpackNull()) {
		%unpack_field(f)
	} else {
		%default_field(f)
	}
			%else
		%unpack_field(f)
			%end
		%else
		_Pac.unpackObject();
		%end
		%n += 1
	%end
	%(n+1).upto(max_id) do |i|
		if(_Length < {{n+1}}) { return; }
		%if f = fields.get_id(i)
		if(!_Pk.tryUnpackNull()) {
			%unpack_field(f)
		} else {
			%default_field(f)
		}
		%else
		_Pac.unpackObject();
		%end
		%n += 1
	%end
	%(n+1).upto(max_id) do |i|
		_Pac.unpackObject();
	%end
	}

	public void messageConvert(Object _Obj) throws MessageTypeException {
		// FIXME
	}
}

