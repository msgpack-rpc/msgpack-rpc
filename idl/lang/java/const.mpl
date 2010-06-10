%nss = doc.namespace(:java)
package {{nss.join('.')}}; %>unless nss.empty?

import java.util.List;
import java.util.Set;
import java.util.Map;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.io.IOException;

public class Constants {
	%data.each do |const|
	public static {{const.type}} {{const.name}};
	%end

	static {
	%data.each do |const|
		%gen_literal(const.type, const.value, const.name)
	%end
	}
}

