%doc = self.doc
%Mplex.file(doc.data[:common_mpl], self)

[%imports(doc, 1)%]

class Server(object):
    # OVERRIDE THESE METHODS.
    %functions.each do |m|
    def {{m.function_name}}(self[%", "+xjoin(m.fields){|a|"#{a.field_name}"} if m.fields%]):
            raise NotImplementedError("{{m.function_name}} not implemented")

    %end


