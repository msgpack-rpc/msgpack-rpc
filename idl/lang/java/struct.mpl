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

public class {{name}} implements MessagePackable, MessageUnpackable, MessageConvertable {
	%if exception?
	// FIXME extends org.msgpack.rpc.RPCException
	// FIXME pack func
	private int code;
	private String message;
	public int getCode() {
		return code;
	}
	public String getMessage() {
		return message;
	}
	%end

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

