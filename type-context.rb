class TypeContext
	LevelMap = {opaque: 0, sizeable: 1, copyable: 2}
	
	LevelLimit = Struct.new(:source, :type, :level) do
		def to_s
			"{#{level.to_s.capitalize}} #{type.text}\n#{source.format(8)}"
		end
	end
	
	def ensure_level(src, type, level)
		raise TypeError.new("#{level.to_s.capitalize} type required. Type '#{type.text}' is a #{type.ref.level} type\n#{src.format}" ) if LevelMap[level] > LevelMap[type.ref.level]
	end
	
	def reduce_level(src, type, level)
		type = type.prune
		
		if type.is_a?(Types::Ref) && type.ref.is_a?(AST::Struct)
			ensure_level(src, type, level)
			true
		end
	end
	
	def require_level(src, type, level)
		unless reduce_level(src, type, level)
			@levels << LevelLimit.new(src, type, level)
		end
	end
	
	class TypeClassLimit
		attr_accessor :source, :typeclass, :args, :eqs
		
		def initialize(source, typeclass, args)
			@source = source
			@typeclass = typeclass
			@args = args
			@eqs = []
		end
		
		def eq_limit(source, var, type_ast)
			@eqs << EqLimit.new(source, var, type_ast)
		end
		
		def to_s
			"#{@typeclass.scoped_name}[#{TypeContext.print_params(@args)}]#{" {#{@eqs.join(', ')}}" unless @eqs.empty?}\n#{source.format(8)}"
		end
	end
	
	EqLimit = Struct.new(:source, :var, :type_ast) do
		def to_s
			".#{type_ast.name} = #{var.real_text}"
		end
	end
	
	attr_accessor :type_vars, :limits, :infer_args, :var_allocs, :levels
		
	def initialize(infer_args)
		@infer_args = infer_args
		@var_allocs = {}
		@type_vars = []
		@var_name = 1
		@limits = []
		@levels = []
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
	
	def occurs_in_list?(t, list)
		list.any? { |arg| occurs_in?(t, arg.prune) }
	end
	
	def occurs_in?(a, b)
		a.require_pruned
		b.require_pruned
		
		return true if a == b
		return false if b.is_a? Types::Variable
		
		return occurs_in_list?(a, b.type_args)
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
	
	def self.print_params(params)
		params.each.map { |p| "#{p.first.scoped_name}: #{p.last.text}" }.join(", ")
	end
	
	Map = Struct.new(:vars, :params, :src) do
		def to_s
			r = "Map{#{TypeContext.print_params(params)}"
			r << " | #{vars.each.map { |p| "#{p.first.text} => #{p.last.text}" }.join(", ")}" unless vars.empty?
			r << "}"
		end
		
		def copy
			self.class.new(vars.dup, params.dup, src)
		end
	end
	
	def src_wrap(src, args)
		AST::NestedSource.new(src, args.src.outer)
	end
	
	def inst_limits(obj, args)
		limits = obj.ctype.ctx.limits.map do |limit|
			class_limit = TypeClassLimit.new(src_wrap(limit.source, args), limit.typeclass, Hash[limit.args.map { |k, v| [k, inst_type(args, v)] }])
			class_limit.eqs = limit.eqs.map do |eq|
				EqLimit.new(src_wrap(eq.source, args), inst_type(args, eq.var), eq.type_ast)
			end
			class_limit
		end
		@limits.concat(limits)
	end
	
	def inst_levels(obj, args)
		levels = obj.ctype.ctx.levels.map do |limit|
			LevelLimit.new(src_wrap(limit.source, args), inst_type(args, limit.type), limit.level)
		end
		@levels.concat(levels)
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Variable
				args.vars[type] ||= new_var(src_wrap(type.source, args), type.name)
			when Types::Ref
				ref = args.params[type.ref]

				if ref
					if type.plain
						ref
					else
						raise unless (ref.is_a?(Types::Ref) && !ref.plain)
						type_args = type.args.map do |k, v|
							[ref.ref.kind.params[type.ref.kind.params.index(k)], inst_type(args, v)]
						end
						parent_args = ref.args.select { |k, v| !ref.ref.kind.params.index(k) }.to_a
						Types::Ref.new(src_wrap(type.source, args), ref.ref, Hash[type_args + parent_args], type.plain)
					end
				else
					Types::Ref.new(src_wrap(type.source, args), type.ref, Hash[type.args.map { |k, v| [k, inst_type(args, v)] }], type.plain)
				end
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst(src, obj, params = {})
		result, args = inst_ex(src, obj, params)
		result
	end
	
	def inst_map(src, obj, params)
		infer(obj)
		inst_args = Map.new({}, params, src)
		inst_limits(obj, inst_args)
		inst_levels(obj, inst_args)
		inst_args
	end
	
	def inst_ex(src, obj, params = {})
		inst_args = inst_map(src, obj, params)
		
		return inst_type(inst_args, obj.ctype.type), inst_args
	end
	
	def unify(a, b, loc = proc { "" })
		a = a.prune
		b = b.prune
		error = proc { raise TypeError.new(errmsg(a, b) + loc.()) }
		
		if a.is_a? Types::Variable
			# Don't unify type variables with themselves TODO: Can this happen?
			return if a == b 
			
			raise TypeError.new(recmsg(a, b) + loc.()) if occurs_in?(a, b)
			a.instance = b
			a.source = a.source || b.source
			return
		end
		
		return unify(b, a, loc) if b.is_a? Types::Variable
		
		error.() if (a.class != b.class)
		
		error.() unless case a
			when Types::Ref
				a.ref == b.ref && a.plain == b.plain
			else
				raise "Unhandled"
		end
		
		a_args = a.type_args
		b_args = b.type_args
		
		error.() if (a_args.size != b_args.size)
		
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
		
		result = Types.cmp_types_args(input, inst) do |input, inst|
			if inst.param # TODO: See how higher-order kinds impacts this
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
	
	def self.find_instance(obj, infer_args, typeclass, args)
		#TODO: Find all matching instances and error if multiple are appliable
		#      If one instance is more specific than the other (or an instance of), use the most specific one.
		#      If we can't tell if we want the specific one, keep the constraint around until it has a fixed type.
		#      Implement this by ensuring all the more specific instances can't be used before picking.
		
		map = nil
		[typeclass.instances.find do |inst|
			next if inst == obj # Instances can't find themselves
			
			InferContext.infer(inst, infer_args)
			result, map = is_instance?(args.values, inst.ctype.typeclass.values)
			
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
				
				c.typeclass == oc.typeclass && c.args == oc.args				
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
	
	def reduce_levels
		@levels.reject! do |l|
			reduced = reduce_level(l.source, l.type, l.level)
			if reduced
				true
			else
				dup = @levels.find do |ol|
					next if l == ol
					l.type == ol.type				
				end
				
				if dup
					dup.level = [dup.level, l.level].max_by { |v| LevelMap[v] }
					true
				end
			end
		end
	end
	
	def reduce(obj)
		# TODO: Add support for superclasses and remove type class constraints that is implied by the superclass of another

		nil while @limits.reject! do |c|
			next if (obj && obj.declared && obj.declared.inside?(c.typeclass.scope)) # Don't search for a typeclass instance inside typeclass declarations
			
			#puts "Searching instance for #{c}"
			inst, map = TypeContext.find_instance(obj, @infer_args, c.typeclass, c.args)
			if inst
				instance = inst(c.source, inst, map) # Adds the limits of the instance to the @limits array
				
				# Resolve the type functions
				c.eqs.each do |eq| 
					ast = instance.ref.scope.names[eq.type_ast.name]
					result = inst(eq.source, ast, instance.args)
					unify(result, eq.var)
				end
				
				true
			elsif c.args.values.all? { |t| t.fixed_type? }
				raise TypeError.new("Unable to find an instance of the type class '#{c}'\n#{c.source.format}")
			end
		end
		
		reduce_limits
		reduce_levels
	end
	
	def dependent_var(map, var, vars)
		return map[var] if map.has_key?(var)
		map[var] = false # It's not dependent if recursive
		
		@limits.each do |c|
			if c.eqs.any? { |eq| occurs_in?(var, eq.var.prune) }
				# All type variables in the arguments to the type class must be dependent for the type function result to be so too
				type_vars = vars.select { |var| occurs_in_list?(var, c.args.values) }
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
			@limits.any? { |c| occurs_in_list?(var, c.args.values) }
		end
	end
end
