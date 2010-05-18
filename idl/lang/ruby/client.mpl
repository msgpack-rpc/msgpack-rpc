%if nss = doc.namespace(:ruby)
module {{ns}}  %|ns| nss.each
%end


class Client < MessagePack::RPC::Client::Base
	%functions.each do |f|
	def {{f.name}}([%join(f.fields){|a|"#{a.name}"}%])
		@client.call(:{{f.name}}[%join(f.fields,''){|a|", #{a.name}"}%])
	end

	def {{f.name}}_async([%join(f.fields){|a|"#{a.name}"}%])
		@client.call_async(:{{f.name}}[%join(f.fields,''){|a|", #{a.name}"}%])
	end
	%end
end


%if nss = doc.namespace(:ruby)
end  # module {{ns}}  %|ns| nss.reverse.each
%end
