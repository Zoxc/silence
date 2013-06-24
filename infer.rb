class InferContext
	attr_accessor :obj, :type, :ctx, :fields, :typeclass, :value, :vars, :infer_args, :dependent_vars
	
	# TODO: How to deal with type specifiers which have limits inside expressions?
	#            Return a list of limits in analyze_impl and use that to check types?
	#            Run them after resolving all constraints?
		
	class TypeError < CompileError
	end

	FieldLimit = Struct.new(:source, :var, :type, :name, :args, :ast, :lvalue) do
		def to_s
			"#{"lvalue" if lvalue} #{var.real_text} = #{type.text}.#{name} [#{args.map{ |t|t .text }.join(", ")}]"
		end
	end
	
	def initialize(infer_args, obj)
		@infer_args = infer_args
		@obj = obj
		@ctx = TypeContext.new(infer_args)
		@vars = {}
		@var_allocs = {}
		@fields = []
		@views = {}
	end
	
	def shared?
		@obj.respond_to?(:props) ? @obj.props[:shared] : true
	end
	
	def ensure_shared(obj, source, args)
		return if args.typeof
		case obj
			when AST::Function, AST::Variable
				raise TypeError.new("Can't access member '#{obj.name}' without an instance\n#{source.format}") unless obj.props[:shared]
		end
	end
	
	def new_var(source = nil, name = nil)
		@ctx.new_var(source, name)
	end
	
	def unit_default(type)
	end
	
	def self.unit_type(source)
		Types::Complex.new(source, Core::Unit, {})
	end
	
	def unit_type(source)
		self.class.unit_type(source)
	end
	
	def self.make_tuple(source, args)
		args.reverse.inject(unit_type(source)) { |m, p| Types::Complex.new(source, Core::Cell::Node, {Core::Cell::Val => p, Core::Cell::Next => m}) }
	end
	
	def make_tuple(source, args)
		self.class.make_tuple(source, args)
	end
	
	def make_ptr(source, type)
		Types::Complex.new(source, Core::Ptr::Node, {Core::Ptr::Type => type})
	end
	
	def typeclass_limit(source, typeclass, args)
		tcl = TypeContext::TypeClassLimit.new(source, typeclass, args)
		@ctx.limits << tcl
		tcl
	end
	
	def req_level(src, type, level = :copyable)
		@ctx.require_level(src, type, level)
	end
	
	class AnalyzeArgs
		attr_accessor :scoped, :index, :lvalue, :tuple_lvalue, :typeof
		def initialize
			@scoped = nil
			@lvalue = false
			@tuple_lvalue = false
			@typeof = false
			@index = {}
		end
		
		def next(opts = {})
			new = dup
			new.scoped = opts[:scoped] || @scoped
			new.typeof = opts[:typeof] || @typeof
			new.lvalue = opts[:lvalue] || false
			new.tuple_lvalue = opts[:tuple_lvalue] || false
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
	
	def analyze_ref(source, obj, analyze_args, parent_args, tc_limit)
		scope = analyze_args.scoped
		args = analyze_args.index
		args[:used] = true
		args = args[:args]
		
		raise TypeError.new("Unexpected type parameter(s) for non-generic object #{obj.scoped_name}\n#{source.format}") if args && !obj.is_a?(AST::Complex) 
		
		args ||= []
		
		case obj
			when AST::TypeClass
				if !scope
					type_class_result = new_var(source)
					args.unshift(type_class_result)
				end
				
				map_type_params(source, parent_args, obj.type_params, args, obj.scoped_name)
				
				result, inst_args = inst_ex(source, obj, parent_args)
			
				limit = typeclass_limit(source, obj, result.args)
				
				if scope
					scope[:complex] = obj
					scope[:limit] = limit
					scope[:args] = limit.args
					result = nil
					inst_args = nil
				else
					@views[type_class_result] = result # TODO: Views won't work nice since type_class_result can be unified with anything
													   #       Generate an error when the view's type variable is unified with anything other than another type variable
													   #       When unifing with another type variable, pick the one with a view. If both have a view, create a type error
													   #       Have the view bind to the variable name instead?
					result = type_class_result
				end
			when AST::Struct
				map_type_params(source, parent_args, obj.type_params, args, obj.scoped_name)
				result, inst_args = inst_ex(source, obj, parent_args)
				
				if scope
					scope[:complex] = obj
					scope[:args] = result.args
					result = nil
					inst_args = nil
				end
			when AST::TypeFunction
				typeclass = obj.declared.owner
				raise "Expected typeclass as owner for type function" unless typeclass.is_a?(AST::TypeClass)
				
				unless tc_limit
					typeclass_type, inst_args = inst_ex(source, typeclass, parent_args)
					tc_limit = typeclass_limit(source, typeclass_type.complex, typeclass_type.args)
				end
				
				result, inst_args = inst_ex(source, obj, parent_args)
				tc_limit.eq_limit(source, result, obj)
				inst_args = TypeContext::Map.new({}, {})
			when AST::Variable, AST::TypeParam, AST::Function, AST::TypeAlias
				result, inst_args = inst_ex(source, obj, parent_args)
			else
				raise "(unhandled #{obj.class})"
		end
		
		lvalue_check(source, obj, analyze_args.lvalue)
		
		[[result, obj.ctype.value], inst_args]
	end
	
	def analyze_value(ast, args)
		type, value = analyze(ast, args)
		ast.gtype = type
		raise TypeError.new("Expected value, but got type '#{type.text}'\n#{type.source.format}") unless value
		type
	end
	
	def analyze_type(ast, args)
		type, value = analyze(ast, args)
		ast.gtype = type
		raise TypeError.new("Expected type, but got value of type '#{type.text}'\n#{type.source.format}") if value
		type
	end
	
	def analyze(ast, args)
		type, value = analyze_impl(ast, args)
		ast.gtype = type
		[type, value]
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
			when AST::Ref, AST::Field, AST::UnaryOp, AST::Tuple
			else
				raise TypeError.new("Invalid l-value (#{ast.class.name})\n#{ast.source.format}")
		end if args.lvalue
		
		case ast
			# Values only
			
			when AST::VariableDecl
				analyze_value(ast.value, args.next) if ast.value
				[unit_type(ast.source), true]
			when AST::Return
				result = analyze_value(ast.value, args.next)
				prev = @result
				
				if prev
					unify(result, prev)
					result
				else
					@result = result
				end
				
				[unit_type(ast.source), true]
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::BoolType)
				
				unit_default analyze_value(ast.group, args.next)
				analyze_value(ast.else_node, args.next) if ast.else_node
				
				[unit_type(ast.source), true]
			when AST::Call
				type, value = analyze(ast.obj, args.next)
				
				result = new_var(ast.source)
				
				callable_args = make_tuple(ast.source, ast.args.map { |arg| analyze_value(arg, args.next) })
				
				if value
					# It's a call
					ast.gen = [true, callable_args]
					
					limit = typeclass_limit(ast.source, Core::Callable::Node, {Core::Callable::T => type})
					limit.eq_limit(ast.source, callable_args, Core::Callable::Args)
					limit.eq_limit(ast.source, result, Core::Callable::Result)
				else
					# It's a constructor
					ast.gen = [false, callable_args]
					
					limit = typeclass_limit(ast.source, Core::Constructor::Node, {Core::Constructor::T => type})
					limit.eq_limit(ast.source, callable_args, Core::Constructor::Args)
					limit.eq_limit(ast.source, result, Core::Constructor::Constructed)
				end
				
				req_level(ast.source, result, :sizeable) # Constraint on Callable.Result / Types must be sizeable for constructor syntax
				
				[result, true]
			when AST::Literal
				[case ast.type
					when :int
						type = new_var(ast.source)
						typeclass_limit(ast.source, Core::IntLiteral::Node, {Core::IntLiteral::T => type})
						type
					when :bool
						Core::Bool.ctype.type
					when :string
						type = new_var(ast.source)
						typeclass_limit(ast.source, Core::StringLiteral::Node, {Core::StringLiteral::T => type})
						type
					else
						raise "Unknown literal type #{ast.type}"
				end, true]
			when AST::Scope
				nodes = ast.nodes.compact
				[if nodes.empty?
					unit_type(ast.source)
				else
					nodes[0...-1].each do |node|
						unit_default analyze_value(node, args.next)
					end
					analyze_value(nodes.last, args.next)
				end, true]
				
			# Value to type
			
			when AST::TypeOf
				type = analyze_value(ast.value, args.next(typeof: true))
				[type, false]
			
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
				if args.lvalue
					raise TypeError.new("Unexpected tuple assignment\n#{ast.source.format}") unless args.tuple_lvalue
					[make_tuple(ast.source, ast.nodes.map { |n| analyze_value(n, args.next(lvalue: true, tuple_lvalue: true)) }), true]
				else
					[make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false]
				end
			when AST::BinOp
				lhs, lvalue = analyze(ast.lhs, args.next(lvalue: ast.op == '=', tuple_lvalue: ast.op == '='))
				rhs, rvalue = analyze(ast.rhs, args.next)
				
				raise TypeError.new("Left side is #{lvalue ? 'a' : 'of'} type '#{lhs.text}'\n#{ast.lhs.source.format}\nRight side is #{rvalue ? 'a' : 'of'} type '#{rhs.text}'\n#{ast.rhs.source.format}") if lvalue != rvalue
				
				if lvalue
					req_level(ast.source, lhs) if ast.op == '='
					
					typeclass = Core::OpMap[ast.op]
					typeclass_limit(ast.source, typeclass[:ref], {typeclass[:param] => lhs}) if typeclass
					
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
							typeclass_limit(ast.source, Core::Tuple::Node, {Core::Tuple::T => lhs})
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
						
						ensure_shared(ast.obj, ast.source, args) if shared?
						
						ast.gen = inst_ex(ast.source, ast.obj, parent_args)
						
						[ast.gen.first, true]
					else
						result, inst_args = analyze_ref(ast.source, ast.obj, args, {}, nil)
						raise TypeError.new("Type '#{result.first.text}' is not a valid l-value\n#{ast.source.format}") if args.lvalue
						ast.gen = [result.first, inst_args]
						result
					end
				end
			when AST::Field
				scope = {}
				type, value = analyze(ast.obj, args.next(scoped: scope, lvalue: args.lvalue))
				
				if value
					result = new_var(ast.source)
					@fields << FieldLimit.new(ast.source, result, type, ast.name, get_index_args(args), ast, args.lvalue)
					[result, true]
				else
					raise TypeError.new("Object doesn't have a scope\n#{ast.obj.source.format}") unless scope[:complex] 
					
					ref = scope[:complex].scope.names[ast.name]
					
					raise TypeError.new("'#{ast.name}' is not a member of '#{scope[:complex].scoped_name}'\n#{ast.source.format}\nScope inferred from:\n#{ast.obj.source.format}") unless ref
					
					ensure_shared(ref, ast.source, args)
					
					result, inst_args = analyze_ref(ast.source, ref, args, scope[:args], scope[:limit])
					
					ast.gen = {type: :single, ref: ref, args: inst_args}
					
					result
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
	
	def inst_ex(src, obj, params = {})
		@ctx.inst_ex(src, obj, params)
	end
	
	def unify(a, b, loc = proc { "" })
		@ctx.unify(a, b, loc)
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
			raise TypeError.new(recmsg(var, type)) if @ctx.occurs_in?(var, type)
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
					
					field_type, inst_args = inst_ex(c.ast.source, field, parent_args)
					
					c.ast.gen = {type: :field, ref: field, args: inst_args}
					
					unify(field_type, var)
					
					true
				when Types::Variable
				else
					raise TypeError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
			end
		end
	end
	
	def process_type_constraint(ast_type, unify_type)
		analyze_args = AnalyzeArgs.new
		
		if ast_type
			type = analyze_type(ast_type, analyze_args)
			raise(TypeError.new("'#{type.text}' is not a type class\n#{ast_type.source.format}")) if type.fixed_type?
			unify(type, unify_type)
		end
	end
	
	def process_type_params
		@obj.type_params.map do |p|
			process_type_constraint(p.type, Types::Param.new(p.source, p))
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
		func_result = Types::Complex.new(func.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => (@result || unit_type(func.source))})

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
	
	def report_unresolved_vars(vars)
		raise TypeError, "Unresolved vars of #{@obj.name}\n#{vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@ctx.var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{@obj.source.format}"
	end
	
	def finalize(type, value)
		type = type.prune
		@value = value
		
		solve_fields
		
		@ctx.reduce_limits
		
		# If we are a complex, we can allow to give a less strict constraints in case @ctx.reduce refers back to it
		if @obj.is_a? AST::Complex
			@type = type
			@obj.ctype = self
		end
		
		@ctx.reduce(@obj)
		
		@fields.each do |c|
			raise TypeError.new("Unable to find field '#{c.name}' in type '#{c.type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{c.type.source.format}" if c.var.source}")
		end
		
		type_vars = @ctx.type_vars.dup
		
		type_vars.reject! { |var| var.instance }
		
		unresolved_vars = type_vars
		
		case @obj
			when AST::Function, AST::TypeFunction
				type_vars = type_vars.select { |var| @ctx.occurs_in?(var, type) }
				unresolved_vars -= type_vars
			else
				type_vars = []
		end
		
		# TODO: Find a proper way to find type variables which doesn't have definition. Done for TypeFunctionLimits, same needed for FieldLimits?
		#         Probably not if we can resolve all FieldLimit before this point (added errors for any remaining field limits)
		#@limits.each do |c|
		#	@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
		#end
		
		# Remove dependent type variables
		@dependent_vars, type_vars = @ctx.find_dependent_vars(unresolved_vars, type_vars)
		unresolved_vars -= @dependent_vars
		
		# Find unresolved type variables used in type class constraints
		unresolved_vars = @ctx.vars_in_typeclass_args(unresolved_vars)
		
		parameterize(type_vars) if @obj.is_a?(AST::Function)
		
		# TODO: Remove type function constraints which contains type variables. We can't have those in the public set of constraints
		
		puts "" unless @ctx.limits.empty? && @ctx.levels.empty? && @views.empty?
		
		puts "#{@obj.scoped_name}  ::  #{type.text}"
		
		@ctx.limits.each{|i| puts "    - #{i}"}
		@ctx.levels.each{|i| puts "    - #{i}"}
		@views.each_pair{|k,v| puts "    * #{k.text} <= #{v.text}"}
		
		report_unresolved_vars(unresolved_vars) unless unresolved_vars.empty?
		
		@type = type
		@obj.ctype = self
	end
	
	def process_instance
		process_type_params

		case @obj
			when Core::Sizeable::Instance
				p = @obj.type_params.first
				p = Types::Param.new(p.source, p)
				req_level(Core.src, p, :sizeable)
			when Core::Copyable::Instance
				p = @obj.type_params.first
				p = Types::Param.new(p.source, p)
				req_level(Core.src, p, :copyable)
		end
		
		typeclass = @obj.typeclass.obj
		analyze_args = AnalyzeArgs.new
		raise TypeError, "Expected #{typeclass.type_params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.type_params.size
		@typeclass = Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.type_params[i], analyze_type(arg, analyze_args)] }]
		finalize(Types::Complex.new(@obj.source, @obj, Hash[@obj.type_params.map { |p| [p, Types::Param.new(p.source, p)] }]), false)
		
		InferContext.infer_scope(@obj.scope, @infer_args)
		
		infer(typeclass)
		
		typeclass.scope.names.each_pair do |name, value|
			next if value.is_a? AST::TypeParam
			member, = @obj.scope.require_with_scope(@obj.source, name, proc { "Expected '#{name}' in instance of typeclass #{typeclass.name}" })
			next if value.is_a? AST::TypeFunction
			m = infer(member)
			
			ctx = TypeContext.new(@infer_args)
			expected_type = ctx.inst(member.source, value, @typeclass)
			ctx.reduce_limits
			ctx.reduce(nil)
			
			raise TypeError, "Expected type '#{expected_type.text}' for #{name} in typeclass instance, but '#{m.text}' found\n#{member.source.format}\nTypeclass definition\n#{value.source.format}" unless m == expected_type
			
			#TODO: How to ensure members are a proper instance of the typeclass?
			#TODO: Figure out how this should work for type parameters in members
			#TODO: Compare the limits of the expected and actual member
		end
	end
	
	def process_fixed(obj)
		finalize(analyze_value(@obj, AnalyzeArgs.new), true)
	end
	
	def create_constructor
		src = proc do @obj.source
			AST::NestedSource.new(@obj.source, Core.src(2))
		end
		
		ref = proc do |node|
			AST::Ref.new(src.(), node)
		end
		type_params = @obj.type_params.map { |tp| AST::TypeParam.new(tp.source, "P_#{tp.name}".to_sym, nil) }
		fields = @obj.scope.names.values.select { |v| v.is_a?(AST::Variable) && !v.props[:shared] }
		
		type_ref = proc do AST::Index.new(src.(), ref.(@obj), type_params.map { |tp| ref.(tp) }) end

		r = AST::Function.new
		
		args = AST::TypeAlias.new(src.(), :Args, AST::Tuple.new(src.(), fields.map { |f| AST::TypeOf.new(src.(), AST::Field.new(src.(), type_ref.(), f.name)) }))
		constructed = AST::TypeAlias.new(src.(), :Constructed, type_ref.())
			
		instance = AST::TypeClassInstance.new(src.(), ref.(Core::Constructor::Node), [type_ref.()], AST::GlobalScope.new([r, args, constructed]), type_params, [])
	
		r.source = src.()
		r.name = :construct
		r.params = [AST::Function::Param.new(src.(), r, :obj, AST::UnaryOp.new(src.(), '*', ref.(constructed))), AST::Function::Param.new(src.(), r, :args, ref.(args))]
		r.type_params = []
		r.result = ref.(Core::Unit)
		fields = fields.map { |field| AST::Field.new(src.(), AST::UnaryOp.new(src.(), '*', AST::NameRef.new(src.(), :obj)), field.name) }
		r.scope = AST::LocalScope.new([AST::BinOp.new(src.(), AST::Tuple.new(src.(), fields), '=', AST::NameRef.new(src.(), :args))])
		r.props = {}
		r
	
		instance.run_pass(:declare_pass)
		instance.run_pass(:sema, true)
		instance.run_pass(:ref_pass)
		
		#puts print_ast(instance)
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
				
				create_constructor if value.is_a?(AST::Struct)
				
				InferContext.infer_scope(value.scope, @infer_args)
			when AST::Variable
				finalize(value.type ? analyze_type(value.type, AnalyzeArgs.new) : new_var(ast.source), true)
			when AST::Function
				if value.scope
					process_function
				else
					process_fixed(value)
				end
			when AST::TypeAlias
				finalize(analyze_type(value.type, AnalyzeArgs.new), false)
			when AST::TypeFunction
				type = new_var(value.source)
				process_type_constraint(value.type, type)
				finalize(type, false)
			else
				raise "Unknown value #{value.class}"
		end
	end
	
	def infer(value)
		InferContext.infer(value, @infer_args)
	end

	def self.infer(value, infer_args)
		return value.ctype.type if value.ctype
		raise "Recursive #{value.scoped_name} - #{value.class}\nStack:\n#{infer_args.stack.reverse.join("\n")}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		infer_args.stack.push(" #{value.scoped_name} - #{value.class}\n#{value.source.format}")
		
		InferContext.new(infer_args, value).process
		
		infer_args.stack.pop
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited, :stack

	def self.infer_scope(scope, infer_args, filter = proc { true })
		scope.names.each_pair do |name, value|
			infer(value, infer_args) if filter.(value)
		end
	end
end
