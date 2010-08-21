%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

require File.join(File.dirname(__FILE__), 'types')

%gen_package(doc) do

%functions.each do |m|
class {{modname(m.function_name)}}
	%gen_struct(m.function_name, m.fields)
end
%end


%end  # gen_package
