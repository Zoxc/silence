class TypeContext
	InstanceStruct = Struct.new(:inst, :map) do
		def to_s
			"Inst{#{inst} (#{map.each.map { |p| "#{p.first} = #{p.last}" }.join(", ")})}"
		end
	end
	
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
	
	attr_accessor :type_vars, :limits, :infer_args, :var_allocs, :limit_corrections
		
	def initialize(infer_args)
		@infer_args = infer_args
		@var_allocs = {}
		@type_vars = []
		@var_name = 1
		@limits = []
		@limit_corrections = {}
	end
	
	def new_var_name
		result = @var_name
		@var_name += 1
		"p#{result}"
	end
	
	def new_var(source = nil, name = nil)
		var = Types::Variable.new(source, self, name ? name.to_s : nil)
		#puts "new var #{var.text}\n#{source.format}"
		#puts "\n#{caller.join("\n")}"
		@type_vars << var
		@var_allocs[var] = caller
		var
	end
	
	def occurs_in?(a, b)
		a.require_pruned
		b.require_pruned
		
		return true if a == b
		return false if b.is_a? Types::Variable
		
		return b.type_args.any? do |arg|
			occurs_in?(a, arg.prune)
		end
	end
	
	def errmsg(a, b)
		return errmsg(b, a) if b.source && !a.source
		
		if a.source && b.source
			"Expression of type '#{a.text}',\n#{a.source.format}\nconflicts with expression of type '#{b.text}',\n#{b.source.format}" 
		elsif a.source
			"Expected type '#{b.text}', but found type '#{a}'\n#{a.source.format}" 
		else
			"Expression of type '#{a.text}', conflicts with expression of type '#{b.text}'" 
		end
	end
	
	def recmsg(a, b)
		source = a.source || b.source
		
		if a.source && b.source
			"Recursive type '#{a.text}',\n#{a.source.format}\ntype '#{b.text}',\n#{b.source.format}" 
		elsif source
			"Recursive type '#{a.text}', occurs in type '#{b.text}'\n#{source.format}" 
		else
			"Recursive type '#{a.text}', occurs in type '#{b.text}'" 
		end
	end
	
	Map = Struct.new(:vars, :params, :limits) do
		def to_s
			"Map(#{vars.each.map { |p| "#{p.first.text} => #{p.last.text}" }.join(", ")} | #{params.each.map { |p| "#{p.first.scoped_name}: #{p.last.text}" }.join(", ")} | #{limits.each.map { |p| "#{p.first} = #{p.last}" }.join(", ")})"
		end
		
		def copy
			self.class.new(vars.dup, params.dup, limits.dup)
		end
	end
	
	def inst_limits(obj, inst_args)
		limits = obj.ctype.ctx.limits.map do |limit|
			class_limit = TypeClassLimit.new(limit.source, inst_type(inst_args, limit.var))
			class_limit.eqs = limit.eqs.map do |eq|
				EqLimit.new(eq.source, inst_type(inst_args, eq.var), eq.type_ast)
			end
			inst_args.limits[limit] = class_limit
			class_limit
		end
		@limits.concat(limits)
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Variable
				args.vars[type] ||= new_var(type.source, type.name)
			when Types::Param
				args.params[type.param] || type
			when Types::Complex
				Types::Complex.new(type.source, type.complex, Hash[type.args.map { |k, v| [k, inst_type(args, v)] }])
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst(obj, params = {}, type_obj = nil)
		result, args = inst_ex(obj, params, type_obj)
		result
	end
	
	def inst_ex(obj, params = {}, type_obj = nil)
		infer(obj)
		inst_args = Map.new({}, params, {})
		
		inst_limits(obj, inst_args)
		
		return inst_type(inst_args, type_obj ? type_obj : obj.ctype.type), inst_args
	end
	
	def unify(a, b, loc = proc { "" })
		a = a.prune
		b = b.prune
		
		if a.is_a? Types::Variable
			# Don't unify type variables with themselves TODO: Can this happen?
			return if a == b 
			
			raise TypeError.new(recmsg(a, b) + loc.()) if occurs_in?(a, b)
			a.instance = b
			a.source = a.source || b.source
			return
		end
		
		return unify(b, a, loc) if b.is_a? Types::Variable
		
		a_args = a.type_args
		b_args = b.type_args
		
		raise TypeError.new(errmsg(a, b) + loc.()) if (a.class != b.class) || (a_args.size != b_args.size)
		
		new_loc = proc do
			source = a.source || b.source
		
			msg = "\n" + if a.source && b.source
				"When unifying types '#{a.text}',\n#{a.source.format}\nand type '#{b.text}',\n#{b.source.format}" 
			elsif source
				"When unifying types '#{a.text}' and type '#{b.text}''\n#{source.format}" 
			else
				"When unifying types '#{a.text}' and type '#{b.text}'" 
			end
			
			msg << loc.()
		end
		
		a_args.each_with_index do |a_arg, i|
			unify(a_arg, b_args[i], new_loc)
		end
	end
	
	def infer(obj)
		InferContext.infer(obj, @infer_args)
	end
	
	def self.is_instance?(input, inst)
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
	
	def self.find_instance(obj, infer_args, input)
		#TODO: Find all matching instances and error if multiple are appliable
		#      If one instance is more specific than the other (or an instance of), use the most specific one.
		#      If we can't tell if we want the specific one, keep the constraint around until it has a fixed type.
		#      Implement this by ensuring all the more specific instances can't be used before picking.
		
		map = nil
		[input.complex.instances.find do |inst|
			next if inst == obj # Instances can't find themselves
			
			InferContext.infer(inst, infer_args)
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
				unify(eq.var, dup.var)
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
				@limit_corrections[c] = dup
				dup.eqs.concat c.eqs
				true
			end
		end
		
		@limits.each do |c|
			reduce_eqs(c.eqs)
		end
	end
	
	# TODO: Turn this into recursion for typeclass instance constraints like solve_limits does
	#          Won't work since we need to have a list of constraints incase we can't solve them all
	def reduce(obj)
		# TODO: Add support for superclasses and remove type class constraints that is implied by the superclass of another

		nil while @limits.reject! do |c|
			next if (obj && obj.declared && obj.declared.inside?(c.var.complex.scope)) # Don't search for a typeclass instance inside typeclass declarations
			
			#puts "Searching instance for #{c}"
			inst, map = TypeContext.find_instance(obj, @infer_args, c.var)
			if inst
				instance, inst_map = inst_ex(inst, map) # Adds the limits of the instance to the @limits array
				@limit_corrections[c] = InstanceStruct.new(instance, inst_map.limits)
				
				# Resolve the type functions
				c.eqs.each do |eq| 
					ast = instance.complex.scope.names[eq.type_ast.name]
					result = inst(ast, instance.args)
					raise "Type function result can't have additional constraints" unless ast.ctype.ctx.limits.empty?
					unify(result, eq.var)
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
			if c.eqs.any? { |eq| occurs_in?(var, eq.var.prune) }
				# All type variables in the arguments to the type class must be dependent for the type function result to be so too
				type_vars = vars.select { |var| occurs_in?(var, c.var) }
				dependent = type_vars.all? { |var| dependent_var(map, var, vars) }
				(return map[var] = true) if dependent
			end
		end
		false
	end
	
	def find_dependent_vars(vars, type_vars)
		map = {}
		dependent_vars = vars.select { |var| dependent_var(map, var, vars) }
		dependent_type_vars = type_vars.select { |var| dependent_var(map, var, type_vars) }
		[dependent_vars + dependent_type_vars, type_vars - dependent_type_vars]
	end
	
	def vars_in_typeclass_args(vars)
		vars.select do |var|
			@limits.any? { |c| occurs_in?(var, c.var) }
		end
	end
end
