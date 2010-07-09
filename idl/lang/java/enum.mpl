%nss = doc.namespace(:java)
package {{nss.join('.')}}; %>unless nss.empty?

import java.io.IOException;
import org.msgpack.Packer;
import org.msgpack.Unpacker;
import org.msgpack.MessagePackable;
import org.msgpack.MessageUnpackable;
import org.msgpack.MessageConvertable;
import org.msgpack.MessageTypeException;

public enum {{name}} implements MessagePackable, MessageUnpackable, MessageConvertable {
	%enum.each do |f|
	{{f.name}}({{f.num}}),
	%end
	;

	private int value;

	private {{name}}() {
		value = 0;
	}

	private {{name}}(int value) {
		this.value = value;
	}

	public int getValue() {
		return value;
	}

	public void messagePack(Packer pk) throws IOException {
		pk.packInt(this.value);
	}

	public void messageUnpack(Unpacker pac) throws IOException, MessageTypeException {
		switch(pac.unpackInt()) {
		%enum.each do |f|
		case {{f.num}}:
			this.value = {{f.num}};
		%end
		default:
			throw new MessageTypeException();
		}
	}

	public void messageConvert(Object obj) throws MessageTypeException {
		if(!(obj instanceof Number)) {
			throw new MessageTypeException();
		}
		switch(((Number)obj).intValue()) {
		%enum.each do |f|
		case {{f.num}}:
			this.value = {{f.num}};
		%end
		default:
			throw new MessageTypeException();
		}
	}
}

