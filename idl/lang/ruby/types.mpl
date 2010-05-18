%if nss = namespace(:ruby)
module {{ns}}  %|ns| nss.each
%end


%each do |d|
%case d
%when AST::Const
{{modname(d.name)}} = {{d.value}}

%when AST::Typedef
%#do nothing

%when AST::Enum
module {{modname(d.name)}}
	%d.fields.each do |f|
	{{modname(f.name)}} = {{f.value}}
	%end
end

%when AST::Struct
class {{modname(d.name)}}
	def initialize(*args)
		%d.fields.each_with_index do |f,i|
		@{{f.name}} = args[{{i}}]
		%end
	end
	attr_accessor [%join(d.fields){|f|":#{f.name}"}%]
	def to_msgpack(out = '')
		[[%join(d.fields){|f|"@#{f.name}"}%]].to_msgpack(out)
	end
	def self.from_msgpack(obj)
		new(*obj)
	end
	%#FIXME id, required, optional
end

%when AST::Exception
class {{modname(d.name)}} < MessagePack::RPC::RemoteError
	def initialize(*args)
		%d.fields.each_with_index do |f,i|
		@{{f.name}} = args[{{i}}]
		%end
	end
	attr_accessor [%join(d.fields){|f|":#{f.name}"}%]
	def to_msgpack(out = '')
		[{{d.fields.map{|f|"@#{f.name}"}.join(', ')}}].to_msgpack(out)
	end
	def self.from_msgpack(obj)
		new(*obj)
	end
	%#FIXME id, required, optional
end

%when AST::Service
	%# do nothing
%end
%end


%if nss = namespace(:ruby)
end  # module {{ns}}  %|ns| nss.reverse.each
%end
