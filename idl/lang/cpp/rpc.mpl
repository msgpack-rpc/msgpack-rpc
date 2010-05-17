#include "types.hpp"
%services.each do |s|
#include "{{s.name}}.hpp"
%end
