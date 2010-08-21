%case data[:mode]
%when :client
#include "{{s.name}}_client.hpp" %|s| services.each
%when :server
#include "{{s.name}}_server.hpp" %|s| services.each
%end
