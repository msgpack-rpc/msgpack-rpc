%Mplex.file(doc.data[:common_mpl], self)
%gen_package(doc)

import java.io.IOException;
import org.msgpack.Packer;
import org.msgpack.Unpacker;
import org.msgpack.MessagePackable;
import org.msgpack.MessageUnpackable;
import org.msgpack.MessageConvertable;
import org.msgpack.MessageTypeException;

public enum {{type_name}} implements MessagePackable {
	%enum.each do |e|
	{{e.field_name}}({{e.num}}),
	%end
	;

	private int value;

	private {{type_name}}() {
		this.value = {{enum.first.num}};
	}

	private {{type_name}}(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}

	public void messagePack(Packer pk) throws IOException {
		pk.packInt(this.value);
	}

	public static Axis messageUnpack(Unpacker pac) throws IOException, MessageTypeException {
		int val = pac.unpackInt();
		switch(val) {
		case {{e.num}}:  return {{e.field_name}}; %|e| enum.each
		default:
			throw new MessageTypeException();
		}
	}

	public static Axis messageConvert(MessagePackObject obj)
			throws MessageTypeException {
		int val = obj.asInt();
		switch(val) {
		case {{e.num}}: return {{e.field_name}}; %|e| enum.each
		default:
			throw new MessageTypeException();
		}
	}
}

