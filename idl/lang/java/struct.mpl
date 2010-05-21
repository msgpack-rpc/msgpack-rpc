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
%	"_A#{$anon_seqid+1}"
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

class {{name}} implements MessagePackable, MessageUnackable, MessageConvertable {
	%fields.each do |f|
	public {{f.type}} {{f.name}};
	%end

	{
	%fields.each do |f|
		%if f.default
%expand_literal(f.type.to_s, f.default, "this.#{f.name}")
		%elsif f.type.integer?
		%#this.{{name}} = 0;
		%elsif f.type.double?
		%#this.{{name}} = 0.0;
		%elsif f.type.bool?
		%#this.{{name}} = true;
		%elsif f.type.bytes?
		this.{{name}} = new byte[0];
		%else
		this.{{name}} = new {{f.type.to_s}}();
		%end
	%end
	}

	public void messagePack(Packer _Pk) throws IOException {
	%if max_id = fields.max_id
		_Pk.packArray({{max_id}});
		%1.upto(max_id) do |i|
			%if f = fields.get_id(i)
		_Pk.pack({{f.name}});
			%else
		_Pk.packNil();
			%end
		%end
	%else
		_Pk.packArray(0);
	%end
	}

	public void messageUnpack(Unpacker _Pac) throws IOException, MessageTypeException {
		int _Length = _Pac.unpackArray();
		// FIXME
		for(int _I=0; _I < _Length; ++_I) {
			_Pac.unpackObject();
		}
	}

	public void messageConvert(Object _Obj) throws MessageTypeException {
		// FIXME
	}
}

