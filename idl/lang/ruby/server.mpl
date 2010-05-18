%if nss = doc.namespace(:ruby)
module {{ns}}  %|ns| nss.each
%end


class Server < MessagePack::RPC::Server::Base
	%# FIXME
	%functions.each do |f|
	#def {{f.name}}([%join(f.fields){|a|"#{a.name}"}%])
	#end
	%end
end


%if nss = doc.namespace(:ruby)
end  # module {{ns}}  %|ns| nss.reverse.each
%end
