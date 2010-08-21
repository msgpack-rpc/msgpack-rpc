%case data[:mode]
%when :client
require File.join(File.basename(__FILE__), "{{s.name}}_client") %|s| services.each
%when :server
require File.join(File.basename(__FILE__), "{{s.name}}_server") %|s| services.each
%end
