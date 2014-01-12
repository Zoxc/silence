class InferContext
	attr_accessor :obj, :type, :ctx, :fields, :typeclass, :vars, :infer_args, :dependent_vars, :value
	
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
		shared = @obj.respond_to?(:props) ? @obj.props[:shared] : true
	end
	
	def ensure_shared(obj, source, args)
		return if args.typeof
		case obj
			when AST::Function, AST::Variable
				return if obj.declared.owner.is_a?(AST::Program)
				raise TypeError.new("Can't access member '#{obj.name}' without an instance\n#{source.format}") unless obj.props[:shared]
		end
	end
	
	def new_var(source = nil, name = nil)
		@ctx.new_var(source, name)
	end
	
	def unit_default(type)
	end
	
	def self.unit_type(source)
		Types::Ref.new(source, Core::Unit, {})
	end
	
	def unit_type(source)
		self.class.unit_type(source)
	end
	
	def self.make_tuple(source, args)
		args.reverse.inject(unit_type(source)) { |m, p| Types::Ref.new(source, Core::Cell::Node, {Core::Cell::Val => p, Core::Cell::Next => m}) }
	end
	
	def make_tuple(source, args)
		self.class.make_tuple(source, args)
	end
	
	def make_ptr(source, type)
		Types::Ref.new(source, Core::Ptr::Node, {Core::Ptr::Type => type})
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
		attr_accessor :lvalue, :tuple_lvalue, :typeof, :typeclass
		def initialize
			@lvalue = false
			@tuple_lvalue = false
			@typeof = false
			@typeclass = false
		end
		
		def next(opts = {})
			new = dup
			new.typeof = opts[:typeof] || @typeof
			new.lvalue = opts[:lvalue] || false
			new.typeclass = opts[:typeclass] || false
			new.tuple_lvalue = opts[:tuple_lvalue] || false
			new
		end
	end
	
	def map_type_params(source, ref, parent_args, args)
		params = ref.type_params
		limit = ref.is_a?(AST::Function) ? ref.type_param_count : params.size
		
		raise TypeError.new("Unexpected type parameter(s) for non-generic object #{ref.scoped_name}\n#{source.format}") if args && params.empty?
		
		args ||= []
		
		raise TypeError.new("Too many type parameter(s) for #{ref.scoped_name}, got #{args.size} but maximum is #{limit}\n#{source.format}") if limit < args.size
		
		args.each_with_index do |arg, i|
			param = params[i]
			parent_args[param] = coerce_typeparam(param, arg)
		end
		
		params.each do |param|
			parent_args[param] ||= new_var(source, param.name)
		end
	end
	
	def lvalue_check(source, obj, lvalue)
		return unless lvalue && obj.is_a?(AST::Function)
		raise TypeError.new("Function '#{obj.name}' is not a valid l-value\n#{source.format}")
	end

	class ASTWrap
		attr_accessor :type
		
		def initialize(type)
			@type = type
		end
	end
	
	def analyze_ref(source, obj, indices, scope, args, parent_args)
		case obj
			when AST::TypeClass
				if !scope
					if args.typeclass
						type_class_result = new_var(source)
						indices ||= []
						indices.unshift(ASTWrap.new(type_class_result))
					else
						raise TypeError.new("Unexpected typeclass\n#{source.format}")
					end
				end
			when AST::TypeFunction
				typeclass = obj.declared.owner
				raise "Expected typeclass as owner for type function" unless typeclass.is_a?(AST::TypeClass)
				
				typeclass_type, inst_args = inst_ex(source, typeclass, parent_args)
				tc_limit = typeclass_limit(source, typeclass_type.ref, typeclass_type.args)
		end
		
		map_type_params(source, obj, parent_args, indices)
		
		result, inst_args = inst_ex(source, obj, parent_args)
		
		case obj
			when AST::TypeClass
				limit = typeclass_limit(source, obj, result.args)
				
				if scope
					# Put a typeclass in Result. It will only be used by AST::Field anyway
					#full_result = TypeclassResult.new(limit)
				else
					@views[type_class_result] = result # TODO: Views won't work nice since type_class_result can be unified with anything
													   #       Generate an error when the view's type variable is unified with anything other than another type variable
													   #       When unifing with another type variable, pick the one with a view. If both have a view, create a type error
													   #       Have the view bind to the variable name instead?
					result = type_class_result
				end
		
			when AST::TypeFunction
				tc_limit.eq_limit(source, result, obj)
				inst_args = TypeContext::Map.new({}, {})
		end
		
		lvalue_check(source, obj, args.lvalue)
		#full_result ? full_result : 
		[Result.new(result, obj.ctype.value), inst_args]
	end
	
	Result = Struct.new(:type, :value) do # TODO: Split into Type and Value results
		def src
			type.source
		end
		
		def to_s
			"Result{#{type}, #{value}}"
		end
	end
	
	RefResult = Struct.new(:ast, :obj, :args) do
		def src
			ast.source
		end
		
		def to_s
			"RefResult{#{obj.scoped_name}}"
		end
	end
	
	ValueFieldResult = Struct.new(:limit) do # TODO: Try to merge this with FieldResult
		def src
			limit.source
		end
		
		def to_s
			"ValueFieldResult{#{limit}}"
		end
	end

	def coerce_typeparam(tp, ast)
		unexpected_value = proc do |source|
			raise TypeError.new("Unexpected value passed as argument for type parameter #{tp.scoped_name}\n#{source.format}")
		end

		result = analyze_impl(ast, AnalyzeArgs.new)
		case result
			when Result
				unexpected_value.(result.type.source) if result.value
				raise TypeError.new("Type parameter #{tp.scoped_name} is not of kind *\n#{result.type.source.format}") if !tp.type_params.empty?
				result.type
			when RefResult
				args = AnalyzeArgs.new
				if tp.type_params.empty?
					type, value = coerce_typeval(result, AnalyzeArgs.new, nil, false)
					unexpected_value.(result.type.source) if value
					type
				else
					# TODO: Check for typeclasses!
					# TODO: Check if kinds match

					obj = result.obj
					infer(obj)
					Types::Ref.new(result.ast.source, obj, result.args, false)
				end
			when ValueFieldResult
				unexpected_value.(result.src)
			else
				raise "(unhandled #{result.class.inspect})"
		end
	end
	
	def coerce_typeval(result, args, index = nil, scoped = false)
		case result
			when Result
				[result.type, result.value]
			when ValueFieldResult
				limit = result.limit
				limit.args = index
				[limit.var, true]
			when RefResult
				resultt, inst_args = analyze_ref(result.ast.source, result.obj, index, scoped, args, result.args)
				
				case result.ast
					when AST::Ref
						result.ast.gen = [resultt.type, inst_args] 
					when AST::Field
						result.ast.gen = {result: resultt.type, type: :single, ref: result.obj, args: inst_args}
				end if resultt.is_a?(Result)

				coerce_typeval(resultt, args)
			else
				raise "(unhandled #{result.class.inspect})"
		end
	end
	
	def analyze_value(ast, args)
		type, value = analyze(ast, args)
		raise TypeError.new("Expected value, but got type '#{type.text}'\n#{type.source.format}") unless value
		type
	end
	
	def analyze_type(ast, args)
		type, value = analyze(ast, args)
		raise TypeError.new("Expected type, but got value of type '#{type.text}'\n#{type.source.format}") if value
		type
	end
	
	def analyze(ast, args)
		type, value = coerce_typeval(analyze_impl(ast, args), args)
		[type, value]
	end
	
	def get_index_args(args)
		if args[:args]
			args[:used] = true
			args[:args]
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
			when ASTWrap
				Result.new(ast.type, false)

			# Values only
			
			when AST::ActionCall
				type = analyze_value(ast.obj, args.next)
				ast.gen = new_var(ast.source)
				unify(make_ptr(ast.source, ast.gen), type)
				
				Result.new(unit_type(ast.source), true)
			when AST::VariableDecl
				ast.gen = @vars[ast.var]
				
				if ast.value
					type = analyze_value(ast.value, args.next)
					unify(ast.gen, type)
				else
					typeclass_limit(ast.source, Core::Defaultable::Node, {Core::Defaultable::T => ast.gen})
				end
				
				Result.new(unit_type(ast.source), true)
			when AST::Return
				result = analyze_value(ast.value, args.next)
				prev = @result
				
				if prev
					unify(result, prev)
					result
				else
					@result = result
				end
				
				Result.new(unit_type(ast.source), true)
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::BoolType)
				
				unit_default analyze_value(ast.group, args.next)
				analyze_value(ast.else_node, args.next) if ast.else_node
				
				Result.new(unit_type(ast.source), true)
			when AST::Call
				type, value = analyze(ast.obj, args.next)
				
				result = new_var(ast.source)
				
				callable_args = make_tuple(ast.source, ast.args.map { |arg| analyze_value(arg, args.next) })
				
				ast.gen = {call: value, args: callable_args, result: result, obj_type: type}
				
				if value
					# It's a call
					limit = typeclass_limit(ast.source, Core::Callable::Node, {Core::Callable::T => type})
					limit.eq_limit(ast.source, callable_args, Core::Callable::Args)
					limit.eq_limit(ast.source, result, Core::Callable::Result)
				else
					# It's a constructor
					limit = typeclass_limit(ast.source, Core::Constructor::Node, {Core::Constructor::T => type})
					limit.eq_limit(ast.source, callable_args, Core::Constructor::Args)
					limit.eq_limit(ast.source, result, Core::Constructor::Constructed)
				end
				
				req_level(ast.source, result, :sizeable) # Constraint on Callable.Result / Types must be sizeable for constructor syntax
				
				Result.new(result, true)
			when AST::Literal
				result = case ast.type
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
				end
				ast.gen = result
				Result.new(result, true)
			when AST::Scope
				nodes = ast.nodes.compact
				result = if nodes.empty?
					unit_type(ast.source)
				else
					nodes[0...-1].each do |node|
						unit_default analyze_value(node, args.next)
					end
					analyze_value(nodes.last, args.next)
				end
				
				Result.new(result, true)
				
			# Value to type
			
			when AST::TypeOf
				type = analyze_value(ast.value, args.next(typeof: true))
				Result.new(type, false)
			
			# The tricky mix of values and types
			
			when AST::Grouped
				Result.new(*analyze(ast.node, args.next))
			when AST::UnaryOp
				raise TypeError.new("Invalid l-value\n#{ast.source.format}") if (ast.op != '*') && args.lvalue
				
				type, value = analyze(ast.node, args.next(lvalue: ast.op == '&'))
				
				case ast.op
					when '*'
						if value
							result = new_var(ast.source)
							ptr = make_ptr(ast.source, result)
							unify(ptr, type)
							ast.gen = result
							Result.new(result, true)
						else
							Result.new(make_ptr(ast.source, analyze_type(ast.node, args.next)), false)
						end
					when '&'
						raise TypeError.new("Can't get the address of types\n#{ast.source.format}") unless value
						result = make_ptr(ast.source, type)
						ast.gen = result
						Result.new(result, true)
					else
						raise TypeError.new("Invalid unary operator '#{ast.op}' allowed\n#{ast.source.format}")
				end
			when AST::Tuple
				if args.lvalue
					raise TypeError.new("Unexpected tuple assignment\n#{ast.source.format}") unless args.tuple_lvalue
					Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_value(n, args.next(lvalue: true, tuple_lvalue: true)) }), true)
				else
					Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false)
				end
			when AST::BinOp
				assign_op = ast.op == '='
				next_args = args.next(lvalue: assign_op, tuple_lvalue: assign_op)
				
				lresult = analyze_impl(ast.lhs, next_args)
				
				lhs, lvalue = coerce_typeval(lresult, next_args)
				rhs, rvalue = analyze(ast.rhs, args.next)
				
				raise TypeError.new("Left side is #{lvalue ? 'a' : 'of'} type '#{lhs.text}'\n#{ast.lhs.source.format}\nRight side is #{rvalue ? 'a' : 'of'} type '#{rhs.text}'\n#{ast.rhs.source.format}") if lvalue != rvalue
				
				if lvalue
					req_level(ast.source, lhs) if (assign_op && !ast.constructing)
					
					typeclass = Core::OpMap[ast.op]
					typeclass_limit(ast.source, typeclass[:ref], {typeclass[:param] => lhs}) if typeclass
					
					unify(lhs, rhs)
					ast.gen = lhs
					Result.new(lhs, true)
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
					
					Result.new(Types::Ref.new(ast.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => rhs}), false)
				end
			when AST::Ref
				result = @vars[ast.obj]
				if result
					ast.gen = result
					Result.new(result, true)
				else
					ensure_shared(ast.obj, ast.source, args) if shared?
					parent_args = Hash[AST.type_params(ast.obj, false).map { |p| [p, map_type(p)] }]
					RefResult.new(ast, ast.obj, parent_args)
				end
			when AST::Field
				next_args = args.next(lvalue: args.lvalue)
				result = analyze_impl(ast.obj, next_args)
				
				type, value = coerce_typeval(result, next_args, nil, true)
				
				if value
					result = new_var(ast.source)
					limit = FieldLimit.new(ast.source, result, type, ast.name, nil, ast, args.lvalue)
					@fields << limit
					ValueFieldResult.new(limit)
				else
					raise TypeError.new("Object doesn't have a scope (#{type})\n#{ast.obj.source.format}") unless type.kind_of?(Types::Ref)
					
					ref = type.ref.scope.names[ast.name]
					
					raise TypeError.new("'#{ast.name}' is not a member of '#{type.ref.scoped_name}'\n#{ast.source.format}\nScope inferred from:\n#{ast.obj.source.format}") unless ref
					
					ensure_shared(ref, ast.source, args)
					
					RefResult.new(ast, ref, type.args)
				end
				
			when AST::Index
				indices = ast.args
				
				result = analyze_impl(ast.obj, args.next(typeclass: args.typeclass))
				
				type, value = case result
					when RefResult, ValueFieldResult
						coerce_typeval(result, args.next, indices)
					else
						raise TypeError.new("[] operator unsupported\n#{ast.source.format}")
				end
				
				Result.new(type, value)
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
			raise TypeError.new(@ctx.recmsg(var, type)) if @ctx.occurs_in?(var, type)
			case type
				when Types::Ref
					field = type.ref.scope.names[c.name]
					
					raise TypeError.new("'#{c.name}' is not a field in type '#{type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless field
					
					lvalue_check(c.ast.source, field, c.lvalue)
				
					parent_args = type.args.dup
					map_type_params(c.source, field, parent_args, c.args)
					
					field_type, inst_args = inst_ex(c.ast.source, field, parent_args)
					
					c.ast.gen = {result: field_type, type: :field, ref: field, args: inst_args}
					
					unify(field_type, var)
					
					true
				when Types::Variable
				else
					raise TypeError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
			end
		end
	end
	
	def process_type_constraint(ast_type, unify_type)
		analyze_args = AnalyzeArgs.new.next(typeclass: true)
		
		if ast_type
			type = analyze_type(ast_type, analyze_args)
			raise(TypeError.new("'#{type.text}' is not a type class\n#{ast_type.source.format}")) if type.fixed_type?
			unify(type, unify_type)
		end
	end
	
	def process_type_params
		@obj.type_params.map do |p|
			process_type_constraint(p.type, Types::Ref.new(p.source, p)) if p.type_params.empty?
		end
	end
	
	def process_function
		func = @obj
		
		analyze_args = AnalyzeArgs.new
		
		process_type_params
		
		@result = (analyze_type(func.result, analyze_args) if func.result)

		var_params = func.params.map(&:var)
		
		func.scope.names.each_value do |var|
			next if var.is_a? AST::TypeParam
			
			@vars[var] = if var.type
					analyze_type(var.type, analyze_args.next(typeclass: var_params.include?(var)))
				else
					new_var(var.source)
				end
		end
		
		func_args = make_tuple(func.source, func.params.map { |p| @vars[p.var] })
		
		analyze(func.scope, AnalyzeArgs.new)
		
		# TODO: make @result default to Unit, so the function can reference itself
		func_result = Types::Ref.new(func.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => (@result || unit_type(func.source))})

		finalize(func_result, true)
	end
	
	def parameterize(tvs)
		t = tvs.map do |tv|
			p = AST::TypeParam.new(tv.source, "%#{tv.text}", AST::KindParams.new(tv.source, [], []), nil, false)
			p.declare_pass(@obj.scope)
			unify(tv, Types::Ref.new(tv.source, p))
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
		
		# If we are a complex, we can allow to give less strict constraints in case @ctx.reduce refers back to it
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
				p = Types::Ref.new(p.source, p)
				req_level(Core.src, p, :sizeable)
			when Core::Copyable::Instance
				p = @obj.type_params.first
				p = Types::Ref.new(p.source, p)
				req_level(Core.src, p, :copyable)
		end
		
		typeclass = @obj.typeclass.obj
		analyze_args = AnalyzeArgs.new
		raise TypeError, "Expected #{typeclass.type_params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.type_params.size
		@typeclass = Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.type_params[i], analyze_type(arg, analyze_args)] }]
		finalize(Types::Ref.new(@obj.source, @obj, Hash[@obj.type_params.map { |p| [p, Types::Ref.new(p.source, p)] }]), false)
		
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
	
	def parent_args
		parent = @obj.scope.parent.owner
		if parent.is_a? AST::Complex
			parent.ctype.type.args.each.to_a
		else
			[]
		end
	end
	
	def map_type(obj, parent = [])
		Types::Ref.new(obj.source, obj, Hash[parent], obj.type_params.empty?)
	end
	
	def create_type(parent)
		finalize(Types::Ref.new(@obj.source, @obj, Hash[@obj.type_params.map { |p| [p, map_type(p)] } + parent]), false)
	end
	
	def process
		value = @obj
		
		case value
			when AST::TypeClassInstance
				process_instance
			when AST::TypeParam
				create_type([])
			when AST::Complex
				process_type_params
				
				create_type(parent_args)
				
				if value.is_a?(AST::Struct)
					Core.create_constructor(value)
					Core.create_def_action_constructor(value) if value.actions[:create]
				end
				
				InferContext.infer_scope(value.scope, @infer_args)
			when AST::Variable
				finalize(value.type ? analyze_type(value.type, AnalyzeArgs.new) : new_var(ast.source), true)
			when AST::Function
				# TODO: Require a fixed type for imports/exports
				process_function
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
		raise "Infer for #{value.scoped_name} during code generation" unless infer_args
		raise "Recursive #{value.scoped_name} - #{value.class}\nStack:\n#{infer_args.stack.reverse.join("\n")}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		infer_args.stack.push(" #{value.scoped_name} - #{value.class}\n#{value.source.format}")
		
		InferContext.new(infer_args, value).process
		
		infer_args.stack.pop
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited, :stack

	def self.infer_scope(scope, infer_args)
		scope.nodes.each do |value|
			case value
				when AST::VariableDecl
					infer(value.var, infer_args)
				else
					infer(value, infer_args)
			end
		end
	end
end
