﻿class InferContext
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
		shared = @obj.respond_to?(:props) ? @obj.props[:shared] : true
	end
	
	def ensure_shared(obj, source, args)
		return if args.typeof
		case obj
			when AST::Function, AST::Variable
				return if @obj.declared.owner.is_a?(AST::Program)
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
		attr_accessor :scoped, :lvalue, :tuple_lvalue, :typeof
		def initialize
			@scoped = nil
			@lvalue = false
			@tuple_lvalue = false
			@typeof = false
		end
		
		def next(opts = {})
			new = dup
			new.scoped = opts[:scoped] || @scoped
			new.typeof = opts[:typeof] || @typeof
			new.lvalue = opts[:lvalue] || false
			new.tuple_lvalue = opts[:tuple_lvalue] || false
			new
		end
	end
	
	def map_type_params(source, parent_args, params, args, desc, limit = params.size)
		args ||= []
		
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
	
	def analyze_ref(source, obj, args, scope, lvalue, parent_args, tc_limit)
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
					full_result = TypeclassResult.new(limit)
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
		
		lvalue_check(source, obj, lvalue)
		
		[full_result ? full_result : Result.new(result, obj.ctype.value), inst_args]
	end
	
	Result = Struct.new(:type, :value) do
		def to_s
			"Result{#{type}, #{value}}"
		end
	end
	
	TypeclassResult = Struct.new(:limit) do
		def to_s
			"TypeclassResult{#{limit}}"
		end
	end
	
	RefResult = Struct.new(:ref) do
		def to_s
			"RefResult{#{ref.obj.scoped_name}}"
		end
	end
	
	ValueFieldResult = Struct.new(:limit) do
		def to_s
			"ValueFieldResult{#{limit}}"
		end
	end
	
	FieldResult = Struct.new(:ast, :field, :args, :limit) do
		def to_s
			"FieldResult{#{parent}, #{field.scoped_name}}"
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
			when TypeclassResult
				raise TypeError.new("Unexpected typeclass\n#{result.limit.source.format}")
			when RefResult
				ast = result.ref
		
				type = infer(ast.obj)
				if ast.obj.ctype.value
					parent_args = {}
					
					if ast.obj.is_a?(AST::Function)
						map_type_params(ast.source, parent_args, ast.obj.type_params, index, ast.obj.name, ast.obj.type_param_count)
					end
					
					lvalue_check(ast.source, ast.obj, args.lvalue)
					
					ensure_shared(ast.obj, ast.source, args) if shared?
					
					ast.gen = inst_ex(ast.source, ast.obj, parent_args)
					
					[ast.gen.first, true]
				else
					result, inst_args = analyze_ref(ast.source, ast.obj, index, scoped, args.lvalue, {}, nil)
					raise TypeError.new("Type '#{result.first.text}' is not a valid l-value\n#{ast.source.format}") if args.lvalue
					ast.gen = [result.type, inst_args] if result.is_a?(Result)
					coerce_typeval(result, args)
				end
			when FieldResult
				ast = result.ast
				
				resultt, inst_args = analyze_ref(ast.source, result.field, nil, scoped, args.lvalue, result.args, result.limit)
				
				ast.gen = {result: resultt, type: :single, ref: result.field, args: inst_args}
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
					
					Result.new(Types::Complex.new(ast.source, Core::Func::Node, {Core::Func::Args => func_args, Core::Func::Result => rhs}), false)
				end
			when AST::Ref
				result = @vars[ast.obj]
				if result
					ast.gen = result
					Result.new(result, true)
				else
					RefResult.new(ast)
				end
			when AST::Field
				next_args = args.next(lvalue: args.lvalue)
				result = analyze_impl(ast.obj, next_args)
				
				case result
					when TypeclassResult
						complex = result.limit.typeclass
						limit = result.limit
						parent_args = result.limit.args
						[nil, false]
					else
						type, value = coerce_typeval(result, next_args, {}, true)
				end
				
				if value
					result = new_var(ast.source)
					limit = FieldLimit.new(ast.source, result, type, ast.name, nil, ast, args.lvalue)
					@fields << limit
					ValueFieldResult.new(limit)
				else
					unless complex
						raise TypeError.new("Object doesn't have a scope (#{type})\n#{ast.obj.source.format}") unless type.kind_of?(Types::Complex)
						
						complex = type.complex
						parent_args = type.args
					end
					
					ref = complex.scope.names[ast.name]
					
					raise TypeError.new("'#{ast.name}' is not a member of '#complex.scoped_name}'\n#{ast.source.format}\nScope inferred from:\n#{ast.obj.source.format}") unless ref
					
					ensure_shared(ref, ast.source, args)
					
					FieldResult.new(ast, ref, parent_args, limit)
				end
				
			when AST::Index
				indices = ast.args.map { |n| analyze_type(n, args.next) }
				
				result = analyze_impl(ast.obj, args.next)
				
				type, value = case result
					when RefResult
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
	
	def create_def_action_constructor
		src = proc do @obj.source
			AST::NestedSource.new(@obj.source, Core.src(2))
		end
		
		ref = proc do |node|
			AST::Ref.new(src.(), node)
		end
		
		type_params = @obj.type_params.map { |tp| AST::TypeParam.new(tp.source, "P_#{tp.name}".to_sym, nil) }

		type_ref = proc do AST::Index.new(src.(), ref.(@obj), type_params.map { |tp| ref.(tp) }) end

		r = AST::Function.new
		
		instance = AST::TypeClassInstance.new(src.(), ref.(Core::Defaultable::Node), [type_ref.()], AST::GlobalScope.new([r]), type_params, [])
	
		r.source = src.()
		r.name = :construct
		r.params = [AST::Function::Param.new(src.(), r, :obj, AST::UnaryOp.new(src.(), '*', type_ref.()))]
		r.type_params = []
		r.result = ref.(Core::Unit)
		r.scope = AST::LocalScope.new([AST::ActionCall.new(src.(), AST::NameRef.new(src.(), :obj), :create)])
		r.props = {}
		r
	
		instance.run_pass(:declare_pass)
		instance.run_pass(:sema, true)
		instance.run_pass(:ref_pass)
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
				
				if value.is_a?(AST::Struct)
					create_constructor
					create_def_action_constructor if value.actions[:create]
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
