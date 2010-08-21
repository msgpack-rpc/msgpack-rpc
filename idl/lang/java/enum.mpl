%Mplex.file(doc.data[:common_mpl], self)
%gen_package(doc)

import java.io.IOException;
import org.msgpack.Packer;
import org.msgpack.Unpacker;
import org.msgpack.MessagePackable;
import org.msgpack.MessageUnpackable;
import org.msgpack.MessageConvertable;
import org.msgpack.MessageTypeException;

public enum {{type_name}} implements MessagePackable, MessageUnpackable, MessageConvertable {
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

	public void messageUnpack(Unpacker pac) throws IOException, MessageTypeException {
		int val = pac.unpackInt();
		switch(val) {
		case {{e.num}}:  %|e| enum.each
			this.value = val;
			break;
		default:
			throw new MessageTypeException();
		}
	}

	public void messageConvert(Object obj) throws MessageTypeException {
		if(!(obj instanceof Number)) {
			throw new MessageTypeException();
		}
		int val = ((Number)obj).intValue();
		switch(val) {
		case {{e.num}}:  %|e| enum.each
			this.value = val;
			break;
		default:
			throw new MessageTypeException();
		}
	}
}

