
ARGS_MAX = 16

class VarlenGenerator
	def initialize
		@n = 1
	end

	def template
		each_join {|i| "typename A#{i}" }
	end

	def args
		each_join {|i| "A#{i} a#{i}" }
	end

	def args_ptr
		each_join {|i| "A#{i}* a#{i}" }
	end

	def args_ref
		each_join {|i| "A#{i}& a#{i}" }
	end

	def args_const_ref
		each_join {|i| "const A#{i}& a#{i}" }
	end

	def types
		each_join {|i| "A#{i}" }
	end

	def types_ptr
		each_join {|i| "A#{i}*" }
	end

	def types_ref
		each_join {|i| "A#{i}&" }
	end

	def types_const_ref
		each_join {|i| "const A#{i}&" }
	end

	def params
		each_join {|i| "a#{i}" }
	end

	def params_ptr
		each_join {|i| "&#{i}*" }
	end

	def each_join(&block)
		Array.new(@n) {|i| block.call(i+1) }.join(', ')
	end

	def each(&block)
		1.upto(@n).each(&block)
		nil
	end

	def run(&block)
		ARGS_MAX.times {
			block.call(self)
			@n += 1
		}
	end
end

def varlen_each(&block)
	VarlenGenerator.new.run(&block)
end

