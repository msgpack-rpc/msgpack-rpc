%doc = self
%Mplex.file(doc.data[:common_mpl], self)

import msgpack
import msgpackrpc

%each do |d|
%case d
%when AST::Constant
{{d.const_name}} = {{d.value}}

%when AST::Enum

class {{d.type_name}}(object):
%d.enum.each do |e|
    {{e.field_name}} = {{e.num}}
%end

    toName = {
%d.enum.each do |e|
      {{e.num}}: '{{e.field_name}}'[%"," if e != d.enum.last%]
%end
    }

    @staticmethod
    def from_msgpack(obj):
        if obj in [ [%d.enum.map{|x|x.num}.join(',')%] ]:
            return {{d.type_name}}(obj)
        else:
            raise TypeError("Invalid enum value")

    def __init__(self, value = {{d.enum.first.field_name}}):
        self._value = value

    def getValue(self):
        return self._value

    def setValue(self, value):
        self._value = value

    value = property(
        fget = getValue,
        fset = setValue)

    def to_msgpack(self, out = ''):
        out = msgpack.packb(value)
        return out

    def __str__(self):
        return "{{d.type_name}}.%s" % toName[self._value]

    def __repr__(self):
        return "{{d.type_name}}.%s" % toName[self._value]

%when AST::Exception
class {{d.type_name}}(msgpackrpc.error.RPCError):
    %gen_struct(d.type_name, d.fields)

%when AST::Struct
class {{d.type_name}}(object):
    %gen_struct(d.type_name, d.fields)

%when AST::Service
    %# done in client.mpl and server.mpl
%end
%end


