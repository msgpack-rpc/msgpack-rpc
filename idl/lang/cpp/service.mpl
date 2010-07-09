%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)
%gen_guard("#{type_name}") do

#include "types.hpp"

%gen_package(doc) do

namespace {{type_name}} {


%functions.each do |m|
struct {{m.function_name}} {
	%gen_struct(m.function_name, m.fields)
};
%end


}  // namespace {{name}}

%end  # gen_package
%end  # gen_guard
