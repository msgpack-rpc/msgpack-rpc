%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

import msgpackrpc
from msgpackrpc.transport import tcp

[%imports(doc, 1)%]

class Client(object):
    def __init__(self, address, timeout=10, loop=None, builder=tcp, reconnect_limit=5, pack_encoding='utf-8', unpack_encoding=None):
        self._base = msgpackrpc.Client(address, timeout=10, loop=None, builder=tcp, reconnect_limit=5, pack_encoding='utf-8', unpack_encoding=None)

    %functions.each do |m|
    def {{m.function_name}}(self[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%]):
        return self._base.call('{{m.function_name}}'[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%])

    def {{m.function_name}}_async(self[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%]):
        return self._base.call_async('{{m.function_name}}'[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%])

    def {{m.function_name}}_notify(self[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%]):
        return self._base.notify('{{m.function_name}}'[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields.size>0%])

    %end



