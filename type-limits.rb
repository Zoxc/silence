class TypeLimits
	attr_accessor :ctx, :limits
	
	class TypeClassLimit
		attr_accessor :source, :var, :eqs
		
		def initialize(source, var)
			@source = source
			@var = var
			@eqs = []
		end
		
		def eq_limit(source, var, type_ast)
			@eqs << EqLimit.new(source, var, type_ast)
		end
		
		def to_s
			"#{@var.text}#{" {#{@eqs.join(', ')}}" unless @eqs.empty?}"
		end
	end
	
	EqLimit = Struct.new(:source, :var, :type_ast) do
		def to_s
			".#{type_ast.name} = #{var.real_text}"
		end
	end
	
	def initialize(ctx)
		@ctx = ctx
		@limits = []
	end
	
	def inst_limits(obj, inst_args)
		limits = obj.ctype.ctx.limits.limits.map do |limit|
			class_limit = TypeClassLimit.new(limit.source, @ctx.inst_type(inst_args, limit.var))
			class_limit.eqs = limit.eqs.map do |eq|
				EqLimit.new(eq.source, @ctx.inst_type(inst_args, eq.var), eq.type_ast)
			end
			class_limit
		end
		@limits.concat(limits)
	end
	
	def unify(a, b)
		@ctx.unify(a, b)
	end
	
	def is_instance?(input, inst)
		map = {}
		
		result = Types.cmp_types(input, inst) do |input, inst|
			if inst.is_a? Types::Param
				if map.key? inst.param
					[true, map[inst.param] == input]
				else
					map[inst.param] = input
					[true, true]
				end
			else
				[input.is_a?(Types::Variable), false]
			end
		end
		
		[result, map]
	end
	
	def find_instance(input)
		#TODO: Find all matching instances and error if multiple are appliable
		#      If one instance is more specific than the other (or an instance of), use the most specific one.
		#      If we can't tell if we want the specific one, keep the constraint around until it has a fixed type.
		#      Implement this by ensuring all the more specific instances can't be used before picking.
		
		map = nil
		[input.complex.instances.find do |inst|
			next if inst == @obj # Instances can't find themselves
			
			@ctx.infer(inst)
			result, map = is_instance?(input, inst.ctype.typeclass)
			
			#puts "Comparing #{inst.ctype.typeclass.text} with #{input.text} = #{result}\n#{input.source.format}"
			
			next unless result
			
			true
		end, map]
	end
	
	def reduce_eqs(eqs)
		eqs.reject! do |eq|
			dup = eqs.find do |oeq|
				next if eq == oeq
				eq.type_ast == oeq.type_ast
			end
			if dup
				@ctx.unify(eq.var, dup.var)
				true
			end
		end
	end
	
	def reduce_limits
		@limits.reject! do |c|
			dup = @limits.find do |oc|
				next if c == oc
				
				c.var == oc.var				
			end
			
			if dup
				dup.eqs.concat c.eqs
				true
			end
		end
		
		@limits.each do |c|
			reduce_eqs(c.eqs)
		end
	end
	
	def reduce(obj)
		# TODO: Add support for superclasses and remove type class constraints that is implied by the superclass of another
		reduce_limits
		
		nil while @limits.reject! do |c|
			next if (obj.declared && obj.declared.inside?(c.var.complex.scope)) # Don't search for a typeclass instance inside typeclass declarations
			
			#puts "Searching instance for #{c}"
			inst, map = find_instance(c.var)
			if inst
				instance = @ctx.inst(inst, map) # Adds the limits of the instance to the @limits array
				
				# Resolve the type functions
				c.eqs.each do |eq| 
					ast = instance.complex.scope.names[eq.type_ast.name]
					result = @ctx.inst(ast, instance.args)
					@ctx.unify(result, eq.var)
				end
				
				true
			elsif c.var.fixed_type?
				raise TypeError.new("Unable to find an instance of the type class '#{c.var.text}'\n#{c.source.format}")
			end
		end
		
		reduce_limits
	end
	
	def dependent_var(map, var, vars)
		return map[var] if map.has_key?(var)
		map[var] = false # It's not dependent if recursive
		
		@limits.each do |c|
			if c.eqs.any? { |eq| @ctx.occurs_in?(var, eq.var.prune) }
				# All type variables in the arguments to the type class must be dependent for the type function result to be so too
				type_vars = vars.select { |var| @ctx.occurs_in?(var, c.var) }
				dependent = type_vars.all? { |var| dependent_var(map, var, vars) }
				(return map[var] = true) if dependent
			end
		end
		false
	end
	
	def find_dependent_vars(vars)
		map = {}
		vars.select { |var| dependent_var(map, var, vars) }
	end
	
	def vars_in_typeclass_args(vars)
		vars.select do |var|
			@limits.any? { |c| @ctx.occurs_in?(var, c.var) }
		end
	end
end
