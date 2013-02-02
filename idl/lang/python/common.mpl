__BEGIN__
def modname(str)
    str[0..0].upcase + str[1..-1]
end

class AST::BuiltInType
    @@typemap = {
        'int8'   => 'Integer',
        'int16'  => 'Integer',
        'int32'  => 'Integer',
        'int64'  => 'Integer',
        'uint8'  => 'Integer',
        'uint16' => 'Integer',
        'uint32' => 'Integer',
        'uint64' => 'Integer',
        'bool'   => 'Float',
        'double' => 'Float',
        'bytes'  => 'String',
        'string' => 'String',
        'list'   => 'Array',
        'set'    => 'Array',  # FIXME
        'map'    => 'Hash',
    }

    def to_s
        @@typemap[type_name]
    end
end

class AST::ExternalType
    def to_s
        @type_name.to_s
    end
end

class AST::ConstName
    def to_s
        self[0..0].upcase + self[1..-1]
    end
end

class AST::EnumFieldName
    def to_s
        self[0..0].upcase + self[1..-1]
    end
end

class AST::ListLiteral
    def to_s
        "[#{value.map{|e|"#{e}"}.join(',')}]"
    end
end

class AST::MapLiteral
    def to_s
        "{#{value.map{|k,v|"#{k} => #{v}"}.join(',')}}"
    end
end

def xjoin(array=self, sep = ', ', &block)
    block ||= Proc.new {|a| a }
    if array.is_a?(Hash)
        array = array.values
    end
    array.map(&block).join(sep)
end

def default_field(f)
    v = f.default
    if f.default.nil?
        v = "#{f.type.type_name}()"
    end
    return "#{f.field_name} = #{v}"
end

def imports(doc, nest)
    out = ""
    dots = '..'
    doc.each do |d|
        case d
        when AST::Constant
            out += "from #{dots}idltypes import #{d.const_name}\n"
        when AST::Enum, AST::Exception, AST::Struct
            out += "from #{dots}idltypes import #{d.type_name}\n"
        end
    end
    return out
end
__END__


%def gen_struct(name, fields)
    %fields.each_value do |f|
    %f = f.field_name
    def get[%f.capitalize%](self):
        return self._{{f}}

    def set[%f.capitalize%](self, value):
        self._{{f}} = value

    [%f.downcase%] = property(
        fget = get[%f.capitalize%],
        fset = set[%f.capitalize%])

    %end

    def __init__(self[%", "+fields.values.map{|d|default_field d}.join(", ") if fields%]):
        %fields.each_value do |f|
        self._{{f.field_name}} = {{f.field_name}}
        %end

    def to_msgpack(self, out = ''):
        %array = Array.new(fields.max_id)
        %1.upto(fields.max_id) do |i|
        %    if f = fields[i]
        %        array[i-1] = "#{f.field_name}"
        %    else
        %        array[i-1] = "None"
        %    end
        %end
        out = [{{xjoin(array.map{|x|"self._#{x}"})}}].to_msgpack(out)
        return out

    def from_msgpack(self, obj):
        if isinstance(obj, list):
            raise TypeError("Not an array")
        if len(obj) < {{fields.max_required_id}}:
            raise TypeError("Not enough fields")

        %1.upto(fields.max_id) do |i|
        %if f = fields[i]
        %if f.type.builtin_type?
        self._{{f.field_name}} = obj[{{i-1}}]
        %else
        self._{{f.field_name}} = {{f.type}}.from_msgpack(obj[{{i-1}}])
        %end
        %end
        %end
%end
