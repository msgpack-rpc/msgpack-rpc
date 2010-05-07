require 'strscan'

class Lexer
	def initialize
		@symbols = []
		@tokens = {}
		@keywords = {}
	end

	def symbol(name, expr)
		@symbols.push [name.to_sym, expr]
	end

	def scan(src)
		tokens = []

		s = StringScanner.new(src)
		until s.empty?
			ok = false
			@symbols.each {|name, expr|
				if m = s.scan(expr)
					next if m.empty?
					pos = s.pos - m.length
					token, value = tokenize(name, m, pos)
					tokens.push [token, value, pos] if value
					ok = true
					break
				end
			}
			unless ok
				raise ScanError, [src, s.pos, "error '#{s.peek(10)}'"]
			end
		end

		tokens

	rescue ScanError
		# FIXME
		puts $!.message[2]
		raise StandardError, $!.to_s
	end

	def token(name, &block)
		@tokens[name.to_sym] = block
	end

	def keyword(str, &block)
		sym = str.to_sym
		block ||= Proc.new {|name| sym }
		@keywords[str.to_s] = block
	end

	private
	def tokenize(name, str, pos)
		if block = @tokens[name]
			value = block.call(str)
			return name, value
		elsif block = @keywords[str]
			value = block.call(name)
			return str, value
		else
			return name, str
		end
	end
end

l = Lexer.new
l.symbol :IntConstant   , /[+-]?[0-9]+/
l.symbol :hexconstant   , /"0x"[0-9A-Fa-f]+/  # FIXME
l.symbol :DubConstant   , /[+-]?[0-9]*(\.[0-9]+)?([eE][+-]?[0-9]+)?/
l.symbol :Identifier    , /[a-zA-Z_][\.a-zA-Z_0-9]*/
l.symbol :whitespace    , /[ \t\r\n]*/
l.symbol :sillycomm     , /\/\*\*\*\*\//
l.symbol :multicomm     , /\/\*[^*]\/\*([^*\/]|[^*]\/|\*[^\/])*\**\*\//
l.symbol :doctext       , /\/\**([^*\/]|[^*]\/|\*[^\/])*\**\*\//
l.symbol :comment       , /\/\/[^\n]*/
l.symbol :unixcomment   , /#[^\n]*/
l.symbol :Symbol        , /[:;\,\{\}\(\)\=<>\[\]\*]/
l.symbol :STIdentifier , /[a-zA-Z-][\.a-zA-Z_0-9-]*/
#l.symbol :literal_begin , /['\"]/
l.symbol :Literal       , /(\'[^\']*\')|(\"[^\"]*\")/

l.token(:whitespace)  {|str| nil }
l.token(:sillycomm)   {|str| nil }
l.token(:multicomm)   {|str| nil }
l.token(:doctext)     {|str| nil }
l.token(:comment)     {|str| nil }
l.token(:unixcomment) {|str| nil }

l.keyword "["
l.keyword "]"
l.keyword "{"
l.keyword "}"
l.keyword "("
l.keyword ")"
l.keyword ":"
l.keyword ";"
l.keyword ","
l.keyword ">"
l.keyword "<"
l.keyword "="
l.keyword "cpp_include"
l.keyword "include"
l.keyword "const"
l.keyword "*"
l.keyword "cpp"
l.keyword "java"
l.keyword "py"
l.keyword "perl"
l.keyword "rb"
l.keyword "php"
l.keyword "cocoa"
l.keyword "csharp"
l.keyword "namespace"
l.keyword "php_namespace"
l.keyword "extends"
l.keyword "service"
l.keyword "required"
l.keyword "optional"
l.keyword "void"
l.keyword "struct"
l.keyword "typedef"
l.keyword "enum"
l.keyword "senum"
l.keyword "throws"
l.keyword "exception"
l.keyword "map"
l.keyword "set"
l.keyword "list"
l.keyword "bool"
l.keyword "byte"
l.keyword "i16"
l.keyword "i32"
l.keyword "i64"
l.keyword "double"
l.keyword "string"
l.keyword "binary"
l.keyword "slist"

Lex = l

