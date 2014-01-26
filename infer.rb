class InferContext
	attr_accessor :obj, :type, :ctx, :fields, :typeclass, :vars, :infer_args, :dependent_vars, :value
	
	# TODO: How to deal with type specifiers which have limits inside expressions?
	#            Return a list of limits in analyze_impl and use that to check types?
	#            Run them after resolving all constraints?
		
	class TypeError < CompileError
	end

	FieldLimit = Struct.new(:source, :var, :type, :name, :args, :ast, :infer_args, :extension, :deref) do
		def to_s
			"#{var.real_text} = #{type.text}.#{name} #{"[#{args.map{ |t|t .text }.join(", ")}]" if args}"
		end
	end
	
	def initialize(infer_args, obj)
		@infer_args = infer_args
		@obj = obj
		@ctx = TypeContext.new(infer_args)
		@vars = {}
		@var_allocs = {}
		@fields = []
		@result = {}
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
	
	def self.scope_lookup(ref, name, extension)
		if extension && extension.parent == ref
			result = extension.scope.names[name]
			return [result, extension] if result
		end

		[ref.scope.names[name], nil]
	end
	
	def scope_lookup(ref, name, extension)
		self.class.scope_lookup(ref, name, extension)
	end
	
	def self.make_ptr(source, type)
		Types::Ref.new(source, Core::Ptr::Node, {Core::Ptr::Type => type})
	end
	
	def make_ptr(source, type)
		self.class.make_ptr(source,type)
	end
	
	def typeclass_limit(source, typeclass, args)
		tcl = TypeContext::TypeClassLimit.new(source, typeclass, args)
		@ctx.limits << tcl
		tcl
	end
	
	def req_level(src, type, level = :copyable)
		@ctx.require_level(src, type, level)
	end
	
	# TODO: Make scoped and typeclass into an enum of NoTypeclasses, ViewTypeclasses, ScopedTypeclasses
	# TODO: Make lvalue and tuple_lvalue into an enum of NotLValue, TupleLValue, LValue
	class AnalyzeArgs
		attr_accessor :lvalue, :tuple_lvalue, :typeof, :typeclass, :scoped, :extended, :func, :unused
		def initialize(func)
			@func = func
			@lvalue = false
			@tuple_lvalue = false
			@typeof = false
			@typeclass = false
			@scoped = false
			@unused = false
			@extended = {}
		end
		
		def next(opts = {})
			new = dup
			new.func = opts[:func] || @func
			new.typeof = opts[:typeof] || @typeof
			new.lvalue = opts[:lvalue] || false
			new.unused = opts[:unused] || false
			new.scoped = opts[:scoped] || false
			new.typeclass = opts[:typeclass] || false
			new.tuple_lvalue = opts[:tuple_lvalue] || false
			new.extended = opts[:extended] || @extended
			new
		end
	end

	def analyze_args
		AnalyzeArgs.new(@obj)
	end
	
	def map_type_params(source, ref, parent_args, args)
		infer(ref) # Functions' type parameters can increase after inference
		
		params = ref.type_params
		limit = ref.is_a?(AST::Function) ? ref.type_param_count : params.size
		
		raise TypeError.new("Unexpected type parameter(s) for non-generic object #{ref.scoped_name}\n#{source.format}") if args && params.empty?
		
		args ||= []
		
		raise TypeError.new("Too many type parameter(s) for #{ref.scoped_name}, got #{args.size} but maximum is #{limit}\n#{source.format}") if limit < args.size
		
		args.each_with_index do |arg, i|
			param = params[i]
			parent_args[param] = coerce_typeparam(param, arg, parent_args)
		end
		
		params.each do |param|
			parent_args[param] ||= new_var(source, param.name)
		end
	end
	
	def lvalue_check(source, obj, lvalue)
		return unless lvalue
		case obj
			when AST::EnumValue
				raise TypeError.new("The enum value '#{obj.name}' is not a valid l-value\n#{source.format}")
			when AST::Function
				raise TypeError.new("Function '#{obj.name}' is not a valid l-value\n#{source.format}")
			when AST::TypeParam
				raise TypeError.new("Type parameter '#{obj.name}' is not a valid l-value\n#{source.format}")
		end
	end

	class ASTWrap
		attr_accessor :type
		
		def initialize(type)
			@type = type
		end
	end
	
	def analyze_ref(source, obj, indices, args, parent_args)
		case obj
			when AST::TypeClass
				if !args.scoped
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
				
				if args.scoped
					# Put a typeclass in Result. It will only be used by AST::Field anyway
					#full_result = TypeclassResult.new(limit)
				else
					@views[type_class_result] = result # TODO: Views won't work nice since type_class_result can be unified with anything
													   #       Generate an error when the view's type variable is unified with anything other than another type variable
													   #       When unifing with another type variable, pick the one with a view. If both have a view, create a type error
													   #       Have the view bind to the variable name instead?
					#result = type_class_result
					full_result = TypeclassResult.new(limit, type_class_result)
				end
		
			when AST::TypeFunction
				tc_limit.eq_limit(source, result, obj)
				inst_args = TypeContext::Map.new({}, {})
		end
		
		lvalue_check(source, obj, args.lvalue)
		[full_result ? full_result : Result.new(result, obj.ctype.value), inst_args]
	end
	
	Result = Struct.new(:type, :value) do # TODO: Split into Type and Value results
		def src
			type.source
		end
		
		def to_s
			"Result{#{type}, #{value}}"
		end
	end
	
	BindingResult = Struct.new(:binding) do
		def src
			type.source
		end
		
		def to_s
			"BindingResult{#{binding.name}}"
		end
	end
	
	TypeclassBinding = Struct.new(:limit, :result) do
		def src
			limit.source
		end
		
		def to_s
			"TypeclassBinding{#{limit}, #{result}}"
		end
	end
	
	TypeclassResult = Struct.new(:limit, :result) do
		def src
			limit.source
		end
		
		def to_s
			"TypeclassResult{#{limit}, #{result}}"
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
	
	ValueFieldResult = Struct.new(:limit) do
		def src
			limit.source
		end
		
		def to_s
			"ValueFieldResult{#{limit}}"
		end
	end

	def eval_ast(ast)
		case ast
			when AST::Ref
				if ast.obj.is_a?(AST::TypeParam) && ast.obj.value && ast.obj.type_params.empty?
					Types::Ref.new(ast.source, ast.obj)
				else
					raise TypeError.new("Unable to evaluate reference at compile-time\n#{ast.source.format}")
				end
			when AST::Literal
				case ast.type
					when :int
						# TODO: Ensure ast.result is a fixed_type and a primitive integer
						Types::Value.new(ast.source, ast.value)
					else
						raise TypeError.new("Unable to evaluate literal at compile-time\n#{ast.source.format}")
				end
			else
				raise TypeError.new("Unable to evaluate expression at compile-time (#{ast.class})\n#{ast.source.format}")
		end
	end

	def coerce_typeparam(tp, ast, parent_args)
		unexpected_value = proc do |source|
			raise TypeError.new("Unexpected value passed as argument for type parameter #{tp.scoped_name}\n#{source.format}")
		end

		result = analyze_impl(ast, analyze_args)

		if tp.type_params.empty?
			type, value = coerce_typeval(result, analyze_args, nil)

			if value
				raise TypeError.new("Unexpected value passed as argument for type parameter #{tp.scoped_name}\n#{type.source}") unless tp.value
				tp_type, inst_args = inst_ex(tp.source, tp, parent_args)
				unify(tp_type, type)

				eval_ast(ast)
			else
				type
			end

		else # Higher-order
			case result
				when RefResult
					# TODO: Check for typeclasses!
					# TODO: Check if kinds match

					obj = result.obj
					infer(obj)
					Types::Ref.new(result.ast.source, obj, result.args, false)
				else
					raise TypeError.new("Expected explicit reference for higher-order type parameter #{tp.scoped_name}\n#{result.src.format}")
			end
		end
	end
	
	def coerce_typeval_impl(result, args, index = nil, extension = nil)
		case result
			when Result
				result
			when ValueFieldResult
				limit = result.limit
				limit.args = index
				Result.new(limit.var, true)
			when BindingResult
				r = @vars[result.binding]
				extended = args.extended[result.binding]
				extension[:ref] = extended if extension
				Result.new(r, true)
			when TypeclassBinding
				extension[:limit] = result.limit if extension
				Result.new(result.result, true)
			when TypeclassResult
				Result.new(result.result, false)
			when RefResult
				resultt, inst_args = analyze_ref(result.ast.source, result.obj, index, args, result.args)

				if resultt.is_a?(Result)
					if result.ast.is_a?(AST::Ref) && result.obj.is_a?(AST::TypeParam) && result.obj.value
						# Include the type param itself in the required map if it's a value
						inst_args.params[result.obj] = map_type(result.obj)
					end

					result.ast.gen = {result: resultt.type, type: :single, ref: result.obj, args: inst_args}
				end 

				resultt
			else
				raise "(unhandled #{result.class.inspect})"
		end
	end
	
	def coerce_typeval(result, args, index = nil, extension = nil)
		result = coerce_typeval_impl(result, args, index, extension) while !result.is_a?(Result)
		[result.type, result.value]
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
			when AST::Ref, AST::Field, AST::UnaryOp, AST::Tuple, AST::Call
			else
				raise TypeError.new("Invalid l-value (#{ast.class.name})\n#{ast.source.format}")
		end if args.lvalue
		
		case ast
			when ASTWrap
				Result.new(ast.type, false)

			when AST::InitEntry
				field = @obj.declared.require(ast.source, ast.field)
				raise "Expected field to construct in #{func.declared.owner}\n#{ast.source.format}" unless field.is_a?(AST::Variable) && field.declared == @obj.declared && !field.props[:shared]
				
				parent_args = Hash[AST.type_params(field, false).map { |p| [p, map_type(p)] }]

				lhs, val = coerce_typeval(RefResult.new(ast, field, parent_args), args)
				rhs = analyze_value(ast.expr, args.next)
				unify(lhs, rhs)
				nil
			when AST::ActionCall
				type = analyze_type(ast.type, args.next)

				action_arg = analyze_value(ast.arg, args.next) if ast.arg

				analyze_value(ast.obj, args.next)
				ast.gen = {type: type, arg: action_arg} 

				Result.new(unit_type(ast.source), true)
			when AST::ActionArgs
				type = analyze_type(ast.obj, args.next).prune
				raise "Unknown action type #{type}" unless type.is_a?(Types::Ref)
				func = type.ref.actions[ast.action_type]
				func_type, inst_args = inst_ex(ast.source, func, type.args)

				result = func_type.args[Core::Func::Args]

				Result.new(result, false)

			# Values only

			when AST::Lambda
				process_func_params(ast.params)

				analyze_value(ast.scope, args.next(func: ast, unused: true))

				func_args = make_tuple(ast.source, ast.params.map { |p| @vars[p.var] })

				func_result = Types::Ref.new(ast.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => (@result[ast] || unit_type(ast.source))})

				ast.gen = func_result

				Result.new(func_result, true)
			when AST::Match
				type = analyze_value(ast.expr, args.next)

				typeclass_limit(ast.source, Core::Eq::Node, {Core::Eq::T => type})

				cases = ast.whens.map do |w|
					w_type = analyze_value(w.type, args.next)
					unify(type, w_type)
					analyze_value(w.group, args.next)
				end

				cases += [analyze_value(ast.else_group, args.next)] if ast.else_group

				cases.inject { |m, o| unify(m, o) } unless args.unused

				ast.gen = {type: type, unused: args.unused}

				Result.new(args.unused ? unit_type(ast.source) : cases.first, true)
			when AST::MatchAs
				type = analyze_value(ast.expr, args.next)

				@vars[ast.rest.binding] = type

				when_objs = ast.rest.whens.map do |w|
					w_type = analyze_impl(w.type, args.next)
					obj = case w_type
						when RefResult
							raise "Expected union case\n#{w.source.format}" unless w_type.obj.is_a?(AST::StructCase)
							unify(type, Types::Ref.new(w.type.source, w_type.obj.parent, w_type.args))
							w_type.obj
						else
							raise "Expected union case\n#{w.source.format}"
					end

					extended = args.extended.dup
					extended[ast.rest.binding] = obj
					
					[obj, analyze_value(w.group, args.next(extended: extended))]
				end

				ast.gen = {expr: type, when_objs: when_objs.map(&:first), unused: args.unused}

				cases = when_objs.map(&:last)

				cases += [analyze_value(ast.rest.else_group, args.next)] if ast.rest.else_group

				cases.inject { |m, o| unify(m, o) } unless args.unused

				Result.new(args.unused ? unit_type(ast.source) : cases.first, true)
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
				prev = @result[args.func]
				
				if prev
					unify(result, prev)
					result
				else
					@result[args.func] = result
				end
				
				Result.new(unit_type(ast.source), true)
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::Ref.new(ast.source, Core::Bool))
				
				l = analyze_value(ast.group, args.next)

				if ast.else_node
					r = analyze_value(ast.else_node, args.next)
					unify(l, r) unless args.unused
				end

				ast.gen = args.unused
				
				Result.new(args.unused ? unit_type(ast.source) : l, true)
			when AST::Call
				next_args = args.next(lvalue: args.lvalue, typeclass: true)
				obj_result = coerce_typeval_impl(analyze_impl(ast.obj, next_args), next_args)

				callable_args = make_tuple(ast.source, ast.args.map { |arg| analyze_value(arg, args.next) })

				if obj_result.is_a?(TypeclassResult)
					unify(callable_args, make_tuple(ast.source, [obj_result.result]))
					ast.gen = {type: :binding, result: obj_result.result}
					TypeclassBinding.new(obj_result.limit, obj_result.result)
				else
					type, value = coerce_typeval(obj_result, next_args)
					
					result = new_var(ast.source)
					
					
					ast.gen = {args: callable_args, result: result, obj_type: type}
					
					ast.gen[:type] = if value
						if args.lvalue
							# It's a indexed assignment
							limit = typeclass_limit(ast.source, Core::Indexable::Node, {Core::Indexable::T => type})
							limit.eq_limit(ast.source, callable_args, Core::Indexable::Index)
							limit.eq_limit(ast.source, result, Core::Indexable::Result)

							ast.gen[:result] = make_ptr(ast.source, ast.gen[:result]) # We return a pointer

							:index
						else
							# It's a call
							limit = typeclass_limit(ast.source, Core::Callable::Node, {Core::Callable::T => type})
							limit.eq_limit(ast.source, callable_args, Core::Callable::Args)
							limit.eq_limit(ast.source, result, Core::Callable::Result)

							:call
						end
					else
						raise TypeError.new("A constructor is not a valid l-value\n#{ast.source.format}") if args.lvalue

						# It's a constructor
						limit = typeclass_limit(ast.source, Core::Constructor::Node, {Core::Constructor::T => type})
						limit.eq_limit(ast.source, callable_args, Core::Constructor::Args)
						limit.eq_limit(ast.source, result, Core::Constructor::Constructed)

						:construct
					end
					
					req_level(ast.source, result, :sizeable) # Constraint on Callable.Result / Types must be sizeable for constructor syntax
					
					Result.new(result, true)
				end
			when AST::Literal
				result = case ast.type
					when :int
						type = new_var(ast.source)
						typeclass_limit(ast.source, Core::IntLiteral::Node, {Core::IntLiteral::T => type})
						type
					when :string
						type = new_var(ast.source)
						typeclass_limit(ast.source, Core::StringLiteral::Node, {Core::StringLiteral::T => type})
						type
					else
						raise "Unknown literal type #{ast.type}"
				end
				ast.gen = result
				Result.new(result, true)
			when AST::ExpressionGroup
				analyze_impl(ast.scope, args)
			when AST::Scope
				ast.names.each_value do |var|
					next unless var.is_a?(AST::Variable)
					next unless var.decl
					
					@vars[var] = if var.type
							analyze_type(var.type, args.next)
						else
							new_var(var.source)
						end
				end
				
				nodes = ast.nodes.compact
				result = if nodes.empty?
					unit_type(ast.owner.source)
				else
					new_args = args.next(unused: true)
					nodes[0...-1].each do |node|
						analyze_value(node, new_args)
					end
					analyze_value(nodes.last, args.next(unused: args.unused))
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
					when '+', '-', '!'
						ast.gen = type
						Result.new(type, true)
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
						raise TypeError.new("Invalid unary operator '#{ast.op}'\n#{ast.source.format}")
				end
			when AST::Tuple
				if args.lvalue
					raise TypeError.new("Unexpected tuple assignment\n#{ast.source.format}") unless args.tuple_lvalue
					Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_value(n, args.next(lvalue: true, tuple_lvalue: true)) }), true)
				else
					Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false)
				end
			when AST::BinOp
				assign_op = Core::AssignOps.include?(ast.op)
				assign_simple = ast.op == '='
				next_args = args.next(lvalue: assign_op, tuple_lvalue: assign_simple)
				
				lresult = analyze_impl(ast.lhs, next_args)
				
				lhs, lvalue = coerce_typeval(lresult, next_args)
				rhs, rvalue = analyze(ast.rhs, args.next)
				
				raise TypeError.new("Left side is #{lvalue ? 'a' : 'of'} type '#{lhs.text}'\n#{ast.lhs.source.format}\nRight side is #{rvalue ? 'a' : 'of'} type '#{rhs.text}'\n#{ast.rhs.source.format}") if lvalue != rvalue
				
				if lvalue
					req_level(ast.source, lhs) if (assign_op && !ast.constructing)

					plain_op = assign_op ? (assign_simple ? '=' : ast.op[0]) : ast.op

					typeclass = Core::OpMap[plain_op]

					result = lhs
					if typeclass
						typeclass_limit(ast.source, typeclass[:ref], {typeclass[:param] => lhs}) 

						result = Types::Ref.new(ast.source, typeclass[:result]) if typeclass[:result]
					end

					unify(lhs, rhs)
					ast.gen = {arg: lhs, result: result, plain_op: plain_op}
					Result.new(result, true)
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
					BindingResult.new(ast.obj)
				elsif ast.obj == @obj
					@result[@obj] = unit_type(@obj.source) unless @result[@obj]
					ast.gen = {type: :self}
					args.func.scope.req_var(@obj.self, @obj.scope) if @obj.self
					lvalue_check(ast.source, ast.obj, args.lvalue)
					Result.new(get_func_type, true)
				else
					ensure_shared(ast.obj, ast.source, args) if shared?
					parent_args = Hash[AST.type_params(ast.obj, false).map { |p| [p, map_type(p)] }]

					case ast.obj
						when AST::Variable, AST::Function
							if ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
								args.func.scope.req_var(@obj.self, @obj.scope)
							end
					end

					RefResult.new(ast, ast.obj, parent_args)
				end
			when AST::Field
				next_args = args.next(lvalue: args.lvalue, scoped: true)
				result = analyze_impl(ast.obj, next_args)
				extension = {}
				
				type, value = coerce_typeval(result, next_args, nil, extension)
				
				if value
					result = new_var(ast.source)
					limit = FieldLimit.new(ast.source, result, type, ast.name, nil, ast, args, extension, [])
					@fields << limit
					ValueFieldResult.new(limit)
				else
					raise TypeError.new("Object doesn't have a scope (#{type})\n#{ast.obj.source.format}") unless type.is_a?(Types::Ref) && type.ref.is_a?(AST::Complex)
					
					ref, ext = scope_lookup(type.ref, ast.name, nil)
					
					raise TypeError.new("'#{ast.name}' is not a member of '#{type.ref.scoped_name}'\n#{ast.source.format}\nScope inferred from:\n#{ast.obj.source.format}") unless ref
					
					ensure_shared(ref, ast.source, args)
					
					RefResult.new(ast, ref, type.args)
				end
				
			when AST::Index
				next_args = args.next(scoped: args.scoped, typeclass: args.typeclass)

				indices = ast.args

				result = analyze_impl(ast.obj, next_args)
				
				case result
					when RefResult, ValueFieldResult
						coerce_typeval_impl(result, next_args, indices)
					else
						raise TypeError.new("[] operator unsupported\n#{ast.source.format}")
				end
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

	def ptr_field(c)
		type = c.type.prune
		inst, map = TypeContext.find_instance(@obj, @infer_args, Core::Reference::Node, {Core::Reference::T => type})
		if inst
			ast = inst.scope.names[:Type]
			result, inst_args = inst_ex(c.ast.source, ast, map)
			c.type = result

			c.deref << {type: type, ref: inst.scope.names[:get], result: make_ptr(c.ast.source, result)}

			ptr_field(c) # More pointers?
			true
		end
	end
	
	def solve_fields
		nil while (changed = false; @fields.reject! do |c|
			var = c.var.prune
			type = c.type.prune
			#type = view(type) if type.is_a? Types::Variable

			limit = c.extension[:limit]
			if limit
				ref = limit.typeclass
				parent_args = limit.args.dup
			else
				case type
					when Types::Ref
						if ptr_field(c)
							changed = true
						else
							ref = type.ref
							parent_args = type.args.dup
						end
					when Types::Variable
						# TODO: Find out when this is really required
						raise TypeError.new(@ctx.recmsg(var, type)) if @ctx.occurs_in?(var, type)
					else
						raise TypeError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
				end
			end

			next nil unless ref

			raise TypeError.new("Object doesn't have a scope (#{type})\n#{c.ast.source.format}") unless ref.is_a?(AST::Complex)

			field, extension = scope_lookup(ref, c.name, c.extension[:ref])
			
			raise TypeError.new("'#{c.name}' is not a field in '#{ref.scoped_name}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless field
			
			lvalue_check(c.ast.source, field, c.infer_args.lvalue)
		
			map_type_params(c.source, field, parent_args, c.args)
			
			field_type, inst_args = inst_ex(c.ast.source, field, parent_args)

			shared = case field
				when AST::Function, AST::Variable
					field.props[:shared]
				else
					true
			end

			if shared
				c.ast.gen = {result: field_type, type: :single_obj, ref: field, args: inst_args}
			else
				c.ast.gen = {result: field_type, type: :field, ref: field, args: inst_args, extension: extension, deref: c.deref}
			end
			
			unify(field_type, var)
			
			true
		end || changed)
	end
	
	def process_type_constraint(ast_type, unify_type)
		a_args = analyze_args.next(typeclass: true)
		
		if ast_type
			type = analyze_type(ast_type, a_args)
			raise(TypeError.new("'#{type.text}' is not a type class\n#{ast_type.source.format}")) if type.fixed_type?
			unify(type, unify_type)
		end
	end
	
	def process_type_params
		@obj.type_params.map do |p|
			process_type_constraint(p.type, Types::Ref.new(p.source, p)) if (p.type_params.empty? && !p.value)
		end
	end

	def get_func_type
		func = @obj
		func_args = make_tuple(func.source, func.params.map { |p| @vars[p.var] })
		func_result = Types::Ref.new(func.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => (@result[func] || unit_type(func.source))})
	end

	def process_func_params(params)
		a_args = analyze_args
		params.map(&:var).each do |var|
			@vars[var] = if var.type
					analyze_type(var.type, a_args.next(typeclass: true))
				else
					new_var(var.source)
				end
		end
	end
	
	def process_function
		func = @obj
		
		a_args = analyze_args
		
		process_type_params
		
		@result[func] = (analyze_type(func.result, a_args) if func.result)

		owner = func.declared.owner

		@vars[func.self] = case owner
			when AST::StructCase
				a_args.extended[func.self] = owner
				owner.parent.ctype.type
			when AST::Struct
				owner.ctype.type
			when AST::TypeClassInstance
				typeclass_obj = owner.typeclass.obj.type_params.first
				owner.ctype.typeclass[typeclass_obj]
			else
				raise "(unhandled #{owner.class})"
		end if func.self
		
		process_func_params(func.params)

		if [:create, :create_args].include? func.action_type
			scope = func.declared
			inits = Hash[func.init_list.map { |i| [scope.names[i.field], true] }]

			func.gen_init_list = scope.nodes.map do |ast|
				case ast
					when AST::VariableDecl
						if !ast.var.props[:shared] && !inits[ast.var]
							var_type, inst_args = inst_ex(func.source, ast.var, owner.ctype.type.args)
							typeclass_limit(ast.var.source, Core::Defaultable::Node, {Core::Defaultable::T => var_type})
							{field: ast.var, type: var_type}
						end
				end
			end.compact
		end

		func.init_list.each { |i| analyze_impl(i, a_args) }
		
		analyze(func.scope, a_args.next(unused: true))
		
		# TODO: make @result default to Unit, so the function can reference itself
		
		finalize(get_func_type, true)
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
		
		# If we are a complex, we can allow to give less strict constraints in case @ctx.reduce refers back to it
		if @obj.is_a? AST::Complex
			@type = type
			@obj.ctype = self
		end

		nil while proc do
			solve_fields
			@ctx.reduce(@obj)
		end.()

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
		# TODO: This allows variable with just p1 types, find a better way to filter type variables
		unresolved_vars = @ctx.vars_in_typeclass_args(unresolved_vars)
		
		parameterize(type_vars) if @obj.is_a?(AST::Function)
		
		puts "" unless @ctx.limits.empty? && @ctx.levels.empty? && @views.empty?
		
		puts "#{@obj.scoped_name}#{"(#{@obj.type_params.map{|k,v|k.name}.join(",")})" unless @obj.type_params.empty?}  ::  #{type.text}"
		
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
		a_args = analyze_args
		raise TypeError, "Expected #{typeclass.type_params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.type_params.size
		@typeclass = Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.type_params[i], analyze_type(arg, a_args)] }]
		finalize(Types::Ref.new(@obj.source, @obj, Hash[@obj.type_params.map { |p| [p, Types::Ref.new(p.source, p)] }]), false)
	end
	
	def map_type(obj, parent = [])
		Types::Ref.new(obj.source, obj, Hash[parent], obj.type_params.empty?)
	end
	
	def create_type
		Types::Ref.new(@obj.source, @obj, Hash[AST.type_params(@obj).map { |p| [p, map_type(p)] }])
	end
	
	def process_req
		value = @obj
		
		case value
			when AST::TypeClassInstance
				process_instance
			when AST::TypeParam
				if value.value
					finalize(analyze_type(value.type, analyze_args), true)
				else
					finalize(create_type, false)
				end
			when AST::Complex
				process_type_params

				finalize(create_type, false)

				case value
					when AST::Enum
						Core.create_enum_eq(value)
					when AST::StructCase
						Core.create_constructor_action(value) unless value.actions[:create_args]
						Core.create_constructor(value)
					when AST::Struct
						unless value.enum?
							Core.create_constructor_action(value) unless value.actions[:create_args]
							Core.create_constructor(value) if value.actions[:create_args]
							Core.create_def_constructor(value) if value.actions[:create]
						end
				end
			when AST::EnumValue
				infer(value.owner)
				finalize(value.owner.ctype.type, true)
			when AST::Variable
				val_ast = value.decl.value if value.props[:field]
				type = analyze_type(value.type, analyze_args) if value.type
				val = analyze_value(val_ast, analyze_args) if val_ast

				type = if type && val
					unify(type, val)
					type
				else
					type || val
				end

				finalize(type, true)
			when AST::Function
				# TODO: Require a fixed type for imports/exports
				process_function
			when AST::TypeAlias
				finalize(analyze_type(value.type, analyze_args), false)
			when AST::TypeFunction
				type = new_var(value.source)
				process_type_constraint(value.type, type)
				finalize(type, false)
			else
				raise "Unknown value #{value.class}"
		end
	end

	def process_all
		raise "process_all before process_req for #{@obj.scoped_name}" unless @obj.ctype

		case @obj
			when AST::TypeClassInstance
				typeclass = @obj.typeclass.obj

				InferContext.infer_scope(@obj.scope, @infer_args)
				
				infer(typeclass)
				
				typeclass.scope.names.each_pair do |name, value|
					next if value.is_a? AST::TypeParam
					member, = @obj.scope.require_with_scope(name)
					raise(CompileError, "Expected '#{name}' in instance of typeclass #{typeclass.scoped_name}\n#{@obj.source.format}") unless member
					next if value.is_a? AST::TypeFunction
					m = infer(member)
					
					ctx = TypeContext.new(@infer_args)
					expected_type = ctx.inst(member.source, value, @typeclass)
					ctx.reduce_limits
					ctx.reduce(nil)
					
					#TODO: Handle tests with type variables here
					#raise TypeError, "Expected type '#{expected_type.text}' for '#{name}' in instance of typeclass #{typeclass.scoped_name}, but '#{m.text}' found\n#{member.source.format}\nTypeclass definition\n#{value.source.format}" unless m == expected_type
					
					#TODO: How to ensure members are a proper instance of the typeclass?
					#TODO: Figure out how this should work for type parameters in members
					#TODO: Compare the limits of the expected and actual member
				end
			when AST::Complex
				InferContext.infer_scope(@obj.scope, @infer_args)
		end
	end
	
	def infer(value)
		InferContext.infer(value, @infer_args)
	end

	def self.process(value, infer_args)
		infer(value, infer_args)
		value.ctype.process_all
	end

	def self.infer(value, infer_args)
		return value.ctype.type if value.ctype
		raise "Infer for #{value.scoped_name} during code generation" unless infer_args
		raise "Recursive #{value.scoped_name} - #{value.class}\nStack:\n#{infer_args.stack.reverse.join("\n")}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		infer_args.stack.push(" #{value.scoped_name} - #{value.class}\n#{value.source.format}")
		
		InferContext.new(infer_args, value).process_req
		
		infer_args.stack.pop
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited, :stack

	def self.infer_scope(scope, infer_args)
		scope.nodes.each do |value|
			case value
				when AST::VariableDecl
					process(value.var, infer_args)
				else
					process(value, infer_args)
			end
		end
	end
end
