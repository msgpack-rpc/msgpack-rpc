%Mplex.file(doc.data[:common_mpl], self)
%gen_package(doc)

import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;
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

public class {{type_name}} implements MessagePackable, MessageUnpackable, MessageConvertable {
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
	public {{f.type}} {{f.field_name}};
	%end

	public {{type_name}}() {
		%fields.each_value do |f|
		%	default_field(f)
		%end
	}

	public static {{type_name}} unpack(Unpacker _Pac) throws IOException {
		{{type_name}} obj = new {{type_name}}();
		obj.messageUnpack(_Pac);
		return obj;
	}

	public static {{type_name}} convert(Object deserialized) {
		{{type_name}} obj = new {{type_name}}();
		if (!(deserialized instanceof MessagePackObject)) throw new MessageTypeException();
		obj.messageConvert((MessagePackObject) deserialized);
		return obj;
	}

	public void messagePack(Packer _Pk) throws IOException {
		_Pk.packArray({{fields.max_id}});
		%1.upto(fields.max_id) do |i|
		%if f = fields[i]
		_Pk.pack({{f.field_name}});
		%else
		_Pk.packNil();
		%end
		%end
	}

	public void messageUnpack(Unpacker _Pac) throws IOException, MessageTypeException {
		int _Length = _Pac.unpackArray();

		if(_Length < {{fields.max_required_id}}) {
			throw new MessageTypeException();
		}

		%1.upto(fields.max_id) do |i|
		%f = fields[i]
		%unless f
			_Pac.unpackObject();
			%next
		%end
		%if f.required?
			%unpack_field(f)
		%elsif i <= fields.max_required_id
			if(!_Pac.tryUnpackNull()) {
				%unpack_field(f)
			}
		%else
			if(_Length > {{i-1}}) {
				%unpack_field(f)
			}
		%end
		%end

		for(int i={{fields.max_id}}; i < _Length; i++) {
			_Pac.unpackObject();
		}
	}

	public void messageConvert(MessagePackObject obj)
	throws MessageTypeException {
		List<MessagePackObject> _Array = obj.asList();
		int _Length = _Array.size();

		if(_Length < {{fields.max_required_id}}) {
			throw new MessageTypeException();
		}

		%1.upto(fields.max_id) do |i|
		%if f = fields[i]

		%if f.required?
			{{f.type.convert_schema(f, "_Array.get(#{i})")}}
		%else
			if(_Length <= {{i-1}}) { return; }  %>if i > fields.max_required_id
			%anon = next_anon
			MessagePackObject {{anon}} = _Array.get({{i}});
			if({{anon}} != null) {
				{{f.type.convert_schema(f, anon)}}
			}
		%end

		%end
		%end
	}

	@Override
	public boolean equals(Object o) {
		if(o instanceof {{type_name}}) {
			return this.equals(({{type_name}})o);
		}
		return false;
	}

	public boolean equals({{type_name}} o) {
		if(o == null) {
			return false;
		}
		%fields.each_value do |f|
		if(!({{gen_equals(f, "o")}})) {
			return false;
		}
		%end
		return true;
	}
}

