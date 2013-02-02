%case data[:mode]
%when :client
import {{data[:pack]}}.{{s.name}} %|s| services.each
#{{data[:pack]}}.{{s.name}}.Client(address) %|s| services.each
%when :server
import {{data[:pack]}}{{s.name}} %|s| services.each
#class {{s.name}}Impl({{data[:pack]}}{{s.name}}.Server) %|s| services.each
%end
