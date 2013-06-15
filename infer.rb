class TypeContext
	attr_accessor :obj, :type, :type_vars, :limits, :fields, :typeclass, :value, :vars, :infer_args, :dependent_vars
	
	class TypeError < CompileError
	end
	
	# TODO: Move limits and variable names into it's own type TypeContext and rename this back to InferContext
	#         How to deal with type specifiers which have limits inside expressions?
	#            Return a list of limits in analyze_impl and use that to check types?
	#            Run them after resolving all constraints?
	
	class Limit
		attr_accessor :source, :var
		
		def initialize(source, var)
			@source = source
			@var = var
		end
	end
	
	class TypeClassLimit < Limit
		attr_accessor :eqs
		
		def initialize(*args)
			super
			@eqs = []
		end
		
		def to_s
			"#{@var.text}#{" {#{@eqs.join(', ')}}" unless @eqs.empty?}"
		end
	end
	
	class EqLimit < Limit
		attr_accessor :type_ast
		
		def initialize(source, var, type_ast)
			super(source, var)
			@type_ast = type_ast
		end
		
		def to_s
			".#{@type_ast.name} = #{@var.real_text}"
		end
	end
	
	class FieldLimit < Limit
		attr_accessor :type, :name, :args, :ast, :lvalue
		
		def initialize(source, var, type, name, args, ast, lvalue)
			super(source, var)
			@type = type
			@name = name
			@args = args
			@ast = ast
			@lvalue = lvalue
		end
		
		def to_s
			"#{"lvalue" if @lvalue} #{@var.real_text} = #{@type.text}.#{@name} [#{args.map{ |t|t .text }.join(", ")}]"
		end
	end
	
	def initialize(infer_args, obj)
		@infer_args = infer_args
		@obj = obj
		@vars = {}
		@var_allocs = {}
		@type_vars = []
		@var_name = 1
		@limits = []
		@fields = []
		@views = {}
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
	
	def shared?
		@obj.respond_to?(:props) ? @obj.props[:shared] : true
	end
	
	def ensure_shared(obj, source)
		case obj
			when AST::Function, AST::Variable
				raise TypeError.new("Can't access member '#{obj.name}' without an instance\n#{source.format}") unless obj.props[:shared]
		end
	end
	
	def unit_default(type)
	end
	
	def make_tuple(source, args)
		args.reverse.inject(Core::Unit.ctype.type.source_dup(source)) { |m, p| Types::Complex.new(source, Core::Cell::Node, {Core::Cell::Val => p, Core::Cell::Next => m}) }
	end
	
	def make_ptr(source, type)
		Types::Complex.new(source, Core::Ptr::Node, {Core::Ptr::Type => type})
	end
	
	class AnalyzeArgs
		attr_accessor :scoped, :index, :lvalue
		def initialize
			@scoped = false
			@lvalue = false
			@index = {}
		end
		
		def next(opts = {})
			new = dup
			new.scoped = opts[:scoped] || @scoped
			new.lvalue = opts[:lvalue] || false
			new.index = opts[:index] || {}
			new
		end
	end
	
	def map_type_params(source, parent_args, params, args, desc, limit = params.size)
		raise TypeError.new("Too many type parameter(s) for #{desc}, got #{args.size} but maximum is #{limit}\n#{source.format}") if limit < args.size
		
		args.each_with_index do |arg, i|
			parent_args[params[i]] = arg
		end
		
		params.each do |param|
			parent_args[param] ||= new_var(source, param.name)
		end
	end
	
	def lvalue_check(source, obj, lvalue)
		return unless lvalue && obj.is_a?(AST::Function)
		raise TypeError.new("Function '#{obj.name}' is not a valid l-value\n#{source.format}")
	end
	
	def analyze_ref(source, obj, analyze_args, parent_args)
		args = analyze_args.index
		args[:used] = true
		args = args[:args]
		type = infer(obj)
		
		if type.kind_of?(Types::Complex)
			args ||= []
			
			if !analyze_args.scoped && type.type_class?
				type_class_result = new_var(source)
				args.unshift(type_class_result)
			end
			
			map_type_params(source, parent_args, obj.type_params, args, type.text)
			
			result, inst_args = inst_ex(obj, parent_args, type)
			result = result.source_dup(source)
			
			if type.type_class?
				@limits << TypeClassLimit.new(source, result)
				if !analyze_args.scoped
					@views[type_class_result] = result # TODO: Views won't work nice since type_class_result can be unified with anything
													   #       Generate an error when the view's type variable is unified with anything other than another type variable
													   #       When unifing with another type variable, pick the one with a view. If both have a view, create a type error
													   #       Have the view bind to the variable name instead?
					result = type_class_result
				end
			end
		elsif args
			raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{source.format}")
		elsif obj.kind_of?(AST::TypeFunction)
			typeclass = obj.declared.owner
			raise "Expected typeclass as owner for type function" unless typeclass.is_a?(AST::TypeClass)
			
			typeclass_type, inst_args = inst_ex(typeclass, parent_args)
			
			class_limit = TypeClassLimit.new(source, typeclass_type) # TODO: Find the typeclass limit for the parent ref AST node
			result = new_var(source)
			@limits << class_limit
			class_limit.eqs << EqLimit.new(source, result, obj)
			inst_args = InstArgs.new({})
		else
			result, inst_args = type, InstArgs.new({})
			result = result.source_dup(source)
		end
		
		lvalue_check(source, obj, analyze_args.lvalue)
		
		[[result, obj.ctype.value], inst_args]
	end
	
	def analyze_value(ast, args)
		ast.gtype = analyze(ast, args)
		type, value = ast.gtype
		raise TypeError.new("Expected value, but got type '#{type.text}'\n#{type.source.format}") unless value
		type
	end
	
	def analyze_type(ast, args)
		ast.gtype = analyze(ast, args)
		type, value = ast.gtype
		raise TypeError.new("Expected type, but got value of type '#{type.text}'\n#{type.source.format}") if value
		type
	end
	
	def analyze(ast, args)
		ast.gtype = analyze_impl(ast, args)
	end
	
	def get_index_args(args)
		if args.index[:args]
			args.index[:used] = true
			args.index[:args]
		else
			[]
		end
	end
	
	def analyze_impl(ast, args)
		case ast
			when AST::Ref, AST::Field, AST::UnaryOp
			else
				raise TypeError.new("Invalid l-value (#{ast.class.name})\n#{ast.source.format}")
		end if args.lvalue
		
		case ast
			# Values only
			
			when AST::Return
				result = analyze_value(ast.value, args.next)
				prev = @result
				
				if prev
					unify(result, prev)
					result
				else
					@result = result
				end
				
				[Core::Unit.ctype.type.source_dup(ast.source), true]
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::BoolType)
				
				unit_default analyze_value(ast.group, args.next)
				analyze_value(ast.else_node, args.next) if ast.else_node
				
				[@unit_type, true]
			when AST::Call
				type = analyze_value(ast.obj, args.next)
				result = new_var(ast.source)
				
				callable_args = make_tuple(ast.source, ast.args.map { |arg| analyze_value(arg, args.next) })
				
				type_class = Types::Complex.new(ast.source, Core::Callable::Node, {Core::Callable::T => type})
				limit = TypeClassLimit.new(ast.source, type_class)
				limit.eqs << EqLimit.new(ast.source, callable_args, Core::Callable::Args)
				limit.eqs << EqLimit.new(ast.source, result, Core::Callable::Result)
				@limits << limit
				[result, true]
			when AST::Literal
				[case ast.type
					when :int
						Core::Int.ctype.type
					when :bool
						Core::Bool.ctype.type
					when :string
						make_ptr(ast.source, Core::Char.ctype.type)
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source), true]
			when AST::Scope
				[if ast.nodes.empty?
					Core::Unit.ctype.type.source_dup(ast.source)
				else
					ast.nodes[0...-1].each do |node|
						unit_default analyze_value(node, args.next)
					end
					analyze_value(ast.nodes.last, args.next)
				end, true]
				
			# The tricky mix of values and types
			
			when AST::Grouped
				analyze(ast.node, args.next)
			when AST::UnaryOp
				raise TypeError.new("Invalid l-value\n#{ast.source.format}") if (ast.op != '*') && args.lvalue
				
				type, value = analyze(ast.node, args.next(lvalue: ast.op == '&'))
				
				case ast.op
					when '*'
						if value
							result = new_var(ast.source)
							ptr = make_ptr(ast.source, result)
							unify(ptr, type)
							[result, true]
						else
							[make_ptr(ast.source, analyze_type(ast.node, args.next)), false]
						end
					when '&'
						raise TypeError.new("Can't get the address of types\n#{ast.source.format}") unless value
						[make_ptr(ast.source, type), true]
					else
						raise TypeError.new("Invalid unary operator '#{ast.op}' allowed\n#{ast.source.format}")
				end
			when AST::Tuple
				[make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false]
			when AST::BinOp
				lhs, lvalue = analyze(ast.lhs, args.next(lvalue: ast.op == '='))
				rhs, rvalue = analyze(ast.rhs, args.next)
				
				raise TypeError.new("Left side is #{lvalue ? 'a' : 'of'} type '#{lhs.text}'\n#{ast.lhs.source.format}\nRight side is #{rvalue ? 'a' : 'of'} type '#{rhs.text}'\n#{ast.rhs.source.format}") if lvalue != rvalue
				
				if lvalue
					unify(lhs, rhs)
					[lhs, true]
				else
					raise TypeError.new("Unknown type operator '#{ast.op}'\n#{ast.source.format}") if ast.op != '->'
					
					func_args = case ast.lhs
						when AST::Grouped
							arg = analyze_type(ast.lhs.node, args.next)
							make_tuple(ast.lhs.source, [arg])
						when AST::Tuple
							lhs
						else
							type_class = Types::Complex.new(ast.source, Core::Tuple::Node, {Core::Tuple::T => lhs})
							@limits << TypeClassLimit.new(ast.source, type_class)
							lhs
							# DONE: Constraint to tuple type instances only! - TODO: Do this is a general way for all typeclass references?
					end				
					
					[Types::Complex.new(ast.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => rhs}), false]
				end
			when AST::Ref
				result = @vars[ast.obj]
				if result
					ast.gen = [result, nil]
					[result, true]
				else
					type = infer(ast.obj)
					if ast.obj.ctype.value
						parent_args = {}
						
						if ast.obj.is_a?(AST::Function)
							map_type_params(ast.source, parent_args, ast.obj.type_params, get_index_args(args), ast.obj.name, ast.obj.type_param_count)
						end
						
						lvalue_check(ast.source, ast.obj, args.lvalue)
						
						ensure_shared(ast.obj, ast.source) if shared?
						
						ast.gen = inst_ex(ast.obj, parent_args)
						
						[ast.gen.first, true]
					else
						result, inst_args = analyze_ref(ast.source, ast.obj, args, {})
						raise TypeError.new("Type '#{result.first.text}' is not a valid l-value\n#{ast.source.format}") if args.lvalue
						ast.gen = [result.first, inst_args]
						result
					end
				end
			when AST::Field
				type, value = analyze(ast.obj, args.next(scoped: true))
				
				if value
					result = new_var(ast.source)
					@fields << FieldLimit.new(ast.source, result, type, ast.name, get_index_args(args), ast, args.lvalue)
					[result, true]
				else
					if type.kind_of? Types::Complex
						ref = type.complex.scope.names[ast.name]
						
						raise TypeError.new("'#{ast.name}' is not a member of type '#{type.text}'\n#{ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless ref
						
						ensure_shared(ref, ast.source)
						
						result, inst_args = analyze_ref(ast.source, ref, args, type.args.dup)
						
						ast.gen = {type: :single, ref: ref, args: inst_args}
						
						result
					else
						raise TypeError.new("Type #{type.text} doesn't have a scope\n#{ast.source.format}")
					end
				end
				
			when AST::Index
				index = {args: ast.args.map { |n| analyze_type(n, args.next) }, used: false}
				type, value = analyze(ast.obj, args.next(index: index))
				
				raise TypeError.new("[] operator unsupported for values\n#{ast.source.format}") if value && !index[:used]
				
				[type, value]
			else
				raise "Unknown AST #{ast.class}"
		end
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
	
	InstArgs = Struct.new(:params) do
		def to_s
			"Inst{#{params.each.map { |p| "#{p.first.scoped_name}: #{p.last.text}" }.join(", ")}}"
		end
		
		def merge(other)
			self.class.new(params.merge(other.params))
		end
		
		def copy
			self.class.new(params.dup)
		end
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Param
				args.params[type.param] || type
			when Types::Complex
				Types::Complex.new(type.source, type.complex, Hash[type.args.map { |k, v| [k, inst_type(args, v)] }])
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst_limits(obj, inst_args)
		obj.ctype.limits.map do |limit|
			class_limit = TypeClassLimit.new(limit.source, inst_type(inst_args, limit.var))
			class_limit.eqs = limit.eqs.map do |eq|
				EqLimit.new(eq.source, inst_type(inst_args, eq.var), eq.type_ast)
			end
			class_limit
		end
	end
	
	def inst(obj, params = {}, type_obj = nil)
		result, args = inst_ex(obj, params, type_obj)
		result
	end
	
	def inst_ex(obj, params = {}, type_obj = nil)
		infer(obj)
		inst_args = InstArgs.new(params)
		
		@limits.concat(inst_limits(obj, inst_args))
		
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
	
	def view(var)
		r = @views.each.find do |pair|
			true if pair.first.prune == var
		end
		r ? r.last : var
	end
	
	def solve_fields
		nil while @fields.reject! do |c|
			var = c.var.prune
			type = c.type.prune
			type = view(type) if type.is_a? Types::Variable
			raise TypeError.new(recmsg(var, type)) if occurs_in?(var, type)
			case type
				when Types::Complex
					field = type.complex.scope.names[c.name]
					
					raise TypeError.new("'#{c.name}' is not a field in type '#{type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless field
					
					lvalue_check(c.ast.source, field, c.lvalue)
				
					if field.is_a?(AST::Function)
						parent_args = type.args.dup
						map_type_params(c.source, parent_args, field.type_params, c.args, field.name, field.type_param_count)
					else
						parent_args = type.args
					end
					
					field_type, inst_args = inst_ex(field, parent_args)
					
					c.ast.gen = {type: :field, ref: field, args: inst_args}
					
					unify(field_type, var)
					
					true
				when Types::Variable
				else
					raise TypeError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
			end
		end
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
			
			infer(inst)
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
				dup.eqs.concat c.eqs
				true
			end
		end
		
		@limits.each do |c|
			reduce_eqs(c.eqs)
		end
	end
	
	def context_reduction
		# TODO: Add support for superclasses and remove type class constraints that is implied by the superclass of another
		reduce_limits
		
		nil while @limits.reject! do |c|
			next if (@obj.declared && @obj.declared.inside?(c.var.complex.scope)) # Don't search for a typeclass instance inside typeclass declarations
			
			#puts "Searching instance for #{c}"
			inst, map = find_instance(c.var)
			if inst
				instance = inst(inst, map) # Adds the limits of the instance to the @limits array
				
				# Resolve the type functions
				c.eqs.each do |eq| 
					ast = instance.complex.scope.names[eq.type_ast.name]
					result = inst(ast, instance.args)
					unify(result, eq.var)
				end
				
				true
			elsif c.var.fixed_type?
				raise TypeError.new("Unable to find an instance of the type class '#{c.var.text}'\n#{c.source.format}")
			end
		end
		
		reduce_limits
	end
	
	def process_type_params
		analyze_args = AnalyzeArgs.new
		
		@obj.type_params.map do |p|
			if p.type
				type = analyze_type(p.type, analyze_args)
				raise(TypeError.new("'#{type.text}' is not a type class\n#{p.type.source.format}")) if type.fixed_type?
				unify(type, Types::Param.new(p.source, p))
			end
		end
	end
	
	def process_function
		func = @obj
		
		analyze_args = AnalyzeArgs.new
		
		process_type_params
		
		@result = (analyze_type(func.result, analyze_args) if func.result)
		
		func.scope.names.each_value do |var|
			next if var.is_a? AST::TypeParam
			
			@vars[var] = if var.type
					analyze_type(var.type, analyze_args)
				else
					new_var(var.source)
				end
		end
		
		func_args = make_tuple(func.source, func.params.map { |p| @vars[p.var] })
		
		analyze(func.scope, AnalyzeArgs.new)
		
		# TODO: make @result default to Unit, so the function can reference itself
		func_result = Types::Complex.new(func.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => (@result || Core::Unit.ctype.type.source_dup(func.source))})

		finalize(func_result, true)
	end
	
	def parameterize(tvs)
		t = tvs.map do |tv|
			p = AST::TypeParam.new(tv.source, "%#{tv.text}", nil)
			p.declare_pass(@obj.scope)
			unify(tv, Types::Param.new(tv.source, p))
			p
		end
		@obj.type_params.concat t
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
	
	def find_dependent_vars(vars)
		map = {}
		vars.select { |var| dependent_var(map, var, vars) }
	end
	
	def unresolved_vars(vars)
		raise TypeError, "Unresolved vars of #{@obj.name}\n#{vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{@obj.source.format}"
	end
	
	def finalize(type, value)
		type = type.prune
		@value = value
		
		solve_fields
		
		context_reduction
		
		@limits.each do |c|
			if c.is_a? FieldLimit
				raise TypeError.new("Unable to find field '#{c.name}' in type '#{c.type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{c.type.source.format}" if c.var.source}")
			end
		end
		
		@type_vars.reject! { |var| var.instance }
		
		unresolved_vars = @type_vars
		
		@type_vars = @type_vars.select { |var| occurs_in?(var, type) }
		
		# Variables aren't allowed to have non-dependent type variables in their type
		unresolved_vars -= @type_vars unless @obj.is_a?(AST::Variable)
		
		parameterize(@type_vars) if @obj.is_a? AST::Function
		
		# TODO: Find a proper way to find type variables which doesn't have definition. Done for TypeFunctionLimits, same needed for FieldLimits?
		#         Probably not if we can resolve all FieldLimit before this point (added errors for any remaining field limits)
		#@limits.each do |c|
		#	@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
		#end
		
		# Remove dependent type variables
		@dependent_vars = find_dependent_vars(unresolved_vars)
		unresolved_vars -= @dependent_vars
		
		# Find unresolved type variables used in type class constraints
		unresolved_vars = unresolved_vars.select do |var|
			@limits.any? { |c| occurs_in?(var, c.var) }
		end
		
		# TODO: Remove type function constraints which contains type variables. We can't have those in the public set of constraints
		
		puts "  #{@obj.scoped_name}  \t::  #{type.text}"
		
		unless @limits.empty?
			@limits.each{|i| puts "    - #{i}"}
		end
		
		unless @views.empty?
			@views.each_pair{|k,v| puts "    * #{k.text} <= #{v.text}"}
		end
		
		unless unresolved_vars.empty?
			raise TypeError, "Unresolved vars of #{@obj.name}\n#{unresolved_vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{obj.source.format}"
		end
		
		@type = type
		@obj.ctype = self
	end
	
	def process_instance
		process_type_params
		
		typeclass = @obj.typeclass.obj
		analyze_args = AnalyzeArgs.new
		raise TypeError, "Expected #{typeclass.type_params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.type_params.size
		@typeclass = Types::Complex.new(@obj.source, typeclass, Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.type_params[i], analyze_type(arg, analyze_args)] }])
		finalize(Types::Complex.new(@obj.source, @obj, Hash[@obj.type_params.map { |p| [p, Types::Param.new(p.source, p)] }]), false)
		
		TypeContext.infer_scope(@obj.scope, @infer_args)
		
		infer(typeclass)
		
		typeclass.scope.names.each_pair do |name, value|
			next if value.is_a? AST::TypeParam
			member, = @obj.scope.require_with_scope(@obj.source, name, proc { "Expected '#{name}' in instance of typeclass #{typeclass.name}" })
			next if value.is_a? AST::TypeFunction
			m = infer(member)
			#TODO: How to ensure members are a proper instance of the typeclass?
			#b = inst(value, @typeclass.args)
			#raise TypeError, "Expected type '#{b.text}' for #{name}, but '#{m.text}' found\n#{member.source.format}" unless m == b
		end
	end
	
	def process_fixed(obj)
		finalize(analyze_value(@obj, AnalyzeArgs.new), true)
	end
	
	def process
		value = @obj
		
		case value
			when AST::TypeClassInstance
				process_instance
			when AST::TypeParam
				finalize(Types::Param.new(value.source, value), false)
			when AST::Complex
				parent = value.scope.parent.owner
				if parent.is_a? AST::Complex
					parent = parent.ctype.type.args.each.to_a
				else
					parent = []
				end
				process_type_params
				finalize(Types::Complex.new(value.source, value, Hash[value.type_params.map { |p| [p, Types::Param.new(p.source, p)] } + parent]), false)
				TypeContext.infer_scope(value.scope, @infer_args)
			when AST::Variable
				finalize(value.type ? analyze_type(value.type, AnalyzeArgs.new) : new_var(ast.source), true)
			when AST::Function
				if value.scope
					process_function
				else
					process_fixed(value)
				end
			when AST::TypeFunction
				finalize(Types::TypeFunc.new(value.source, value), false)
			else
				raise "Unknown value #{value.class}"
		end
	end
	
	def infer(value)
		TypeContext.infer(value, @infer_args)
	end

	def self.infer(value, infer_args) # TODO: Should have a source argument for better recursive error messages
		return value.ctype.type if value.ctype
		raise "Recursive #{value.scoped_name} - #{value.class}\nStack:\n#{infer_args.stack.reverse.join("")}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		infer_args.stack.push(" #{value.scoped_name} - #{value.class}\n#{value.source.format}")
		
		TypeContext.new(infer_args, value).process
		
		infer_args.stack.pop
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited, :stack

	def self.infer_scope(scope, infer_args)
		scope.names.each_pair do |name, value|
			infer(value, infer_args)
		end
	end
end
