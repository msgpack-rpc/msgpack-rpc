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
	%fields.each do |f|
	{{f.name}}({{f.value}}),
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
		%fields.each do |f|
		case {{f.value}}:
			this.value = {{f.value}};
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
		%fields.each do |f|
		case {{f.value}}:
			this.value = {{f.value}};
		%end
		default:
			throw new MessageTypeException();
		}
	}
}

