
module Types
	def self.cmp_types(a, b, &cmp)
		a = a.prune
		b = b.prune
		
		done, result = cmp.(a, b)
		return result if done
		
		return false unless a.class == b.class
		
		return false unless case a
			when Types::Variable
				a.equal?(b)
			when Types::Complex
				a.complex == b.complex
			when Types::Param
				a.param == b.param
		end
		
		return cmp_types_args(a.type_args, b.type_args, &cmp)
	end
	
	def self.cmp_types_args(a, b, &cmp)
		return false if a.size != b.size
		
		return a.zip(b).all? do |arg|
			cmp_types(arg.first, arg.last, &cmp)
		end
	end
	
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
			Types.cmp_types(self, other) { [false, nil] }
		end
		
		def to_s
			text
		end
		
		def inspect
			text
		end
		
		def require_pruned
		end
		
		def source_dup(source)
			result = dup
			result.source = source
			result
		end
	end

	class Variable < Type
		attr_accessor :instance, :name
		
		def initialize(source, ctx, name)
			@source = source
			@ctx = ctx
			@name = name
		end
		
		def require_pruned
			raise "Unpruned" if @instance
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
				(@name_num ||= @ctx.new_var_name) + (@name ? "_" + @name : '')
			end
		end
		
		def real_text
			text
			#"#{@name_num ||= @ctx.new_var_name} {#{text}}"
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

	class Complex < Type
		attr_accessor :complex, :args
		
		def initialize(source, complex, args)
			@source = source
			@complex = complex
			@args = args
			
			raise "Expected hash" unless @args.is_a? Hash
			raise "Expected param" unless @args.keys.all? { |k| k.is_a? AST::TypeParam }
			raise "Expected type" unless @args.values.all? { |v| v.is_a? Type }
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
				when Core::Unit
					[]
				when Core::Cell::Node
					[@args[Core::Cell::Val], *@args[Core::Cell::Next].tuple_map]
				else
					raise "Expected tuple type"
			end
		end
		
		def text
			case complex
				when Core::Func::Node
					"#{@args[Core::Func::Args].text} -> #{@args[Core::Func::Result].text}"
				when Core::Ptr::Node
					"*#{@args[Core::Ptr::Type].text}"
				when Core::Unit, Core::Cell::Node
					"(#{tuple_map.map(&:text).join(', ')})"
				else
					"#{@complex.scoped_name}#{"[#{@args.map { |k, v| "#{k.name}: #{v.text}" }.join(", ")}]" if @args.size > 0}"
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
		
		def fixed_type?
			false
		end

		def text
			"#{@param.scoped_name}"
		end
	end
end
