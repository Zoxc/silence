
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
			when Types::Ref
				a.ref == b.ref and a.plain == b.plain
			when Types::Value
				a.value == b.value
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
		
		def param
			nil
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
	end

	class Variable < Type
		attr_accessor :instance, :name
		
		def initialize(source, ctx, name)
			@source = source
			@ctx = ctx
			@name = name
		end
		
		def stack
			@ctx.var_allocs[self][0..3].join("\n")
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
	end
	
	def self.verify_args(ref, args, plain)
		raise "Expected hash" unless args.is_a? Hash
		raise "Expected type" unless args.values.all? { |v| v.is_a? Type }

		raise "Not a higher-kind #{ref.scoped_name}" if (!plain && ref.type_params.empty?)

		params = AST.type_params(ref, plain)

		args.keys.all? do |k|
			raise "Expected param" unless k.is_a?(AST::TypeParam)
			raise "Unexpected param #{k.scoped_name} for #{ref.scoped_name}, (#{params.map(&:scoped_name).join(', ')}) allowed" unless params.index(k)
		end
		params.each do |p|
			raise "Missing param #{p.scoped_name} for #{ref.scoped_name}, got (#{args.keys.map(&:scoped_name).join(', ')}) plain:#{plain}" unless args[p]
		end
	end

	class Ref < Type
		attr_accessor :ref, :args, :plain
		
		def initialize(source, ref, args = {}, plain = true)
			raise "Missing source for #{ref.scoped_name}" unless source
			@source = source
			@ref = ref
			@args = args
			args.freeze
			@plain = plain

			case ref
				when AST::TypeParam, AST::Complex
				else
					raise "Invalid type #{ref.class}"
			end
			
			Types.verify_args(ref, args, plain)
		end
		
		def param
			@ref if @ref.is_a?(AST::TypeParam)
		end
		
		def type_args
			@args.values
		end
		
		def fixed_type?
			!param && @args.values.all? { |v| v.fixed_type? }
		end
		
		def tuple_map
			case @ref
				when Core::Unit
					[]
				when Core::Cell::Node
					[@args[Core::Cell::Val], *@args[Core::Cell::Next].tuple_map]
				when param
					[self]
				else
					raise "Expected tuple type (got #{self})"
			end
		end
		
		def text
			case @ref
				when Core::Func::Node
					"#{@args[Core::Func::Args].text} -> #{@args[Core::Func::Result].text}"
				when Core::Ptr::Node
					"*#{@args[Core::Ptr::Type].text}"
				when Core::Unit, Core::Cell::Node
					"(#{tuple_map.map(&:text).join(', ')})"
				else
					"#{"!" unless @plain}#{@ref.scoped_name}#{"(#{@args.map { |k, v| "#{k.name}: #{v.text}" }.join(", ")})" if @args.size > 0}"
			end
		end
	end

	class Value < Type
		attr_accessor :value
		
		def initialize(source, value)
			@source = source
			@value = value
		end

		def text
			"::#{value}"
		end
	end
end
