﻿
module Types
	class Type
		attr_accessor :source
		
		def text
			"<error>"
		end
		
		def real_text
			text
		end
		
		def type_args
			[]
		end
		
		def prune
			self
		end
		
		def template?
			false
		end
		
		def fixed_type?
			type_args.all? { |t| t.fixed_type? }
		end

		def ==(other)
			other = other.prune
			
			return if other.class != self.class
			equal?(other) || rest_eql(other)
		end
		
		def rest_eql(other)
			args = type_args
			other_args = other.type_args
			
			return args == other_args
		end
		
		def source_dup(source)
			result = dup
			result.source = source
			result
		end
	end

	class Variable < Type
		attr_accessor :instance
		
		def initialize(source, system, instance)
			@source = source
			@system = system
			@instance = instance
		end
		
		def ==(other)
			if @instance
				return @instance == other
			else
				super
			end
		end
		
		def rest_eql(other)
			false
		end
		
		def prune
			if @instance
				pruned = @instance.prune
				@source = @source || pruned.source
				@instance = pruned
			else
				self
			end
		end
		
		def type_args
			@instance ? @instance.type_args : []
		end

		def fixed_type?
			@instance.fixed_type? if @instance
		end

		def text
			if @instance
				@instance.text
			else
				@name ||= @system.new_var_name
			end
		end
		
		def real_text
			text
			#"#{@name ||= @system.new_var_name} {#{text}}"
		end
		
		def source
			if @instance
				@instance.source
			else
				@source
			end
		end
		
		def source_dup(source)
			self
		end
	end

	class Ptr < Type
		attr_accessor :type
		
		def initialize(source, type)
			@source = source
			@type = type
		end
		
		def type_args
			[@type]
		end
		
		def text
			"*#{@type.text}"
		end
	end
	
	class Function < Type
		attr_accessor :args, :result
		
		def initialize(source, args, result)
			@source = source
			@args = args
			@result = result
		end
		
		def type_args
			[@args, @result]
		end
		
		def text
			"{#{@args.text} -> #{@result.text}}"
		end
	end
	
	class Complex < Type
		attr_accessor :complex, :args
		
		def initialize(source, complex, args)
			@source = source
			@complex = complex
			@args = args
			
			raise "Expected hash" unless @args.is_a? Hash
			raise "Expected param" unless @args.keys.all? { |k| k.is_a? AST::Complex::Param }
			raise "Expected type" unless @args.values.all? { |v| v.is_a? Type }
		end
		
		def rest_eql(other)
			return false if @complex != other.complex
			super
		end
		
		def type_args
			@args.values
		end
		
		def fixed_type?
			@args.values.all? { |v| v.fixed_type? }
		end
		
		def type_class?
			@complex.type_class?
		end

		def tuple_map
			case complex
				when AST::Unit
					[]
				when AST::Cell::Node
					[@args[AST::Cell::Val], *@args[AST::Cell::Next].tuple_map]
			end
		end
		
		def text
			case complex
				when AST::Unit, AST::Cell::Node
					"(#{tuple_map.map(&:text).join(', ')})"
				else
					"#{@complex.name}#{"[#{@args.map { |k, v| "#{k.name}: #{v.text}" }.join(", ")}]" if @args.size > 0}"
			end
		end
	end
	
	class Param < Type
		attr_accessor :param
		
		def initialize(source, param)
			@source = source
			@param = param
		end
		
		def tuple_map
			[self]
		end
		
		def text
			"param #{@param.name}"
		end
	end
	
	class TypeFunc < Type
		attr_accessor :func
		
		def initialize(source, func)
			@source = source
			@func = func
		end
		
		def text
			"type_func #{@func.name}"
		end
	end
end
