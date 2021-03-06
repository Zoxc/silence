﻿class InferContext
	attr_accessor :obj, :type, :fields, :typeclass, :vars, :infer_args, :dependent_vars, :value
	
	# TODO: How to deal with type specifiers which have limits inside expressions?
	#            Return a list of limits in analyze_impl and use that to check types?
	#            Run them after resolving all constraints?
	
	FieldLimit = Struct.new(:source, :var, :type, :name, :args, :ast, :infer_args, :extension, :deref) do
		def to_s
			"#{var.real_text} = #{type.text}.#{name} #{"[#{args.map{ |t|t .text }.join(", ")}]" if args}"
		end
	end

	def ctx
		@public_ctx
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
				raise CompileError.new("Can't access member '#{obj.name}' without an instance\n#{source.format}") unless obj.props[:shared]
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
	
	def req_level(src, type, copyable = true)
		if copyable
			typeclass_limit(src, Core::Copyable::Node, {Core::Copyable::T => type})
		else
			typeclass_limit(src, Core::Sizeable::Node, {Core::Sizeable::T => type})
		end
	end
	
	# TODO: Make scoped and typeclass into an enum of NoTypeclasses, ViewTypeclasses, ScopedTypeclasses
	# TODO: Make lvalue and tuple_lvalue into an enum of NotLValue, TupleLValue, LValue
	class AnalyzeArgs
		attr_accessor :lvalue, :tuple_lvalue, :typeof, :typeclass, :scoped, :extended, :func, :loop, :unused, :instance
		def initialize(func)
			@func = func
			@lvalue = false
			@tuple_lvalue = false
			@typeof = false
			@typeclass = false
			@scoped = false
			@unused = false
			@instance = false
			@extended = {}
		end
		
		def next(opts = {})
			new = dup
			new.func = opts[:func] || @func
			new.loop = opts[:loop] || @loop
			new.typeof = opts[:typeof] || @typeof
			new.instance = opts[:instance] || @instance
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
	
	def map_type_params(source, ref, parent_args, indices, args)
		infer(ref) # Functions' type parameters can increase after inference
		
		params = ref.type_params
		limit = ref.is_a?(AST::Function) ? ref.type_param_count : params.size
		
		raise CompileError.new("Unexpected type parameter(s) for non-generic object #{ref.scoped_name}\n#{source.format}") if indices && params.empty?
		
		indices ||= []
		
		raise CompileError.new("Too many type parameter(s) for #{ref.scoped_name}, got #{indices.size} but maximum is #{limit}\n#{source.format}") if limit < indices.size
		
		values = ref.kind_params.values
		processed = {}

		default_param = nil

		def_param = proc do |t|
			t = t.ref
			if params.include?(t)
				default_param.(t)
			else
				parent_args[t]
			end
		end

		inst_type_param = proc do |param|
			inst_args = @ctx.new_inst_args(TypeContext::Map.new({}, parent_args, source), def_param)
			@ctx.inst_map_args(param, inst_args, true)
			@ctx.inst_type(inst_args, param.ctype.type)
		end

		default_param = proc do |param|
			next parent_args[param] if parent_args.has_key?(param)

			raise CompileError.new("Recursive default type parameter value\n#{param.source.format}") if processed[param]
		
			processed[param] = true

			value = values[param]
			parent_args[param] = if value
				inst_type_param.(value)
			else
				new_var(source, param.scoped_name.to_s.gsub(".", "__"))
			end
		end

		indices.each_with_index do |arg, i|
			param = params[i]
			type = coerce_typeparam(param, arg, args, inst_type_param)
			if parent_args[param]
				unify(type, parent_args[param])
			else
				parent_args[param] = type
			end
		end
		
		params.each { |param| default_param.(param) }
	end
	
	def lvalue_check(source, obj, lvalue)
		return unless lvalue
		case obj
			when AST::EnumValue
				raise CompileError.new("The enum value '#{obj.name}' is not a valid l-value\n#{source.format}")
			when AST::Function
				raise CompileError.new("Function '#{obj.name}' is not a valid l-value\n#{source.format}")
			when AST::TypeParam
				raise CompileError.new("Type parameter '#{obj.name}' is not a valid l-value\n#{source.format}")
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
						raise CompileError.new("Unexpected typeclass\n#{source.format}")
					end
				end
			when AST::TypeFunction
				typeclass = obj.declared.owner
				raise "Expected typeclass as owner for type function" unless typeclass.is_a?(AST::TypeClass)
				
				typeclass_type, inst_args = inst_ex(source, typeclass, parent_args)
				tc_limit = typeclass_limit(source, typeclass_type.ref, typeclass_type.args)
		end
		
		map_type_params(source, obj, parent_args, indices, args)

		result, inst_args = inst_ex(source, obj, parent_args, !args.instance)

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
			when AST::Ref, AST::Field
				case ast.gen[:type]
					when :single
						ref = ast.gen[:ref]
						case ref
							when AST::EnumValue
								Types::Value.new(ast.source, ref.owner.values.index(ref))
							when AST::TypeParam
								Types::Ref.new(ast.source, ref)
						end
					else
						raise CompileError.new("Unable to evaluate reference at compile-time\n#{ast.source.format}")
				end
			when AST::Literal
				case ast.type
					when :string
						raise CompileError.new("Unable to evaluate string literal at compile-time\n#{ast.source.format}") if ast.value.size != 1
						# TODO: Ensure ast.result is a fixed_type and a primitive integer
						Types::Value.new(ast.source, ast.value.ord)
					when :int
						# TODO: Ensure ast.result is a fixed_type and a primitive integer
						Types::Value.new(ast.source, ast.value)
					else
						raise CompileError.new("Unable to evaluate literal at compile-time\n#{ast.source.format}")
				end
			else
				raise CompileError.new("Unable to evaluate expression at compile-time (#{ast.class})\n#{ast.source.format}")
		end
	end

	def coerce_typeparam(tp, ast, args, inst_type_param)
		result = analyze_impl(ast, args)

		if tp.type_params.empty?
			type, value = coerce_typeval(result, args, nil)

			if value
				raise CompileError.new("Unexpected value passed as argument for type parameter #{tp.scoped_name}\n#{type.source}") unless tp.value
				tp_type = inst_type_param.(tp)
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
					raise CompileError.new("Expected explicit reference for higher-order type parameter #{tp.scoped_name}\n#{result.src.format}")
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
		raise CompileError.new("Expected value, but got type '#{type.text}'\n#{type.source.format}") unless value
		type
	end
	
	def analyze_type(ast, args)
		type, value = analyze(ast, args)
		raise CompileError.new("Expected type, but got value of type '#{type.text}'\n#{type.source.format}") if value
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
			when AST::Ref, AST::Field, AST::UnaryOp, AST::ValueTuple, AST::Call, AST::Subscript, AST::Grouped
			else
				raise CompileError.new("Invalid l-value (#{ast.class.name})\n#{ast.source.format}")
		end if args.lvalue
		
		case ast
			when ASTWrap
				Result.new(ast.type, false)

			# Types only

			when AST::TypeTuple
				Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false)


			when AST::InitEntry
				field = @obj.declared.require(ast.source, ast.field)
				is_field = field.is_a?(AST::Variable) && !field.props[:shared]
				is_owner_field = field.declared == @obj.declared || (@obj.declared.owner.is_a?(AST::StructCase) && field.declared.owner == @obj.declared.owner.parent)
				
				raise "Expected field to construct in #{@obj.declared.owner.scoped_name}, got #{field.scoped_name}\n#{ast.source.format}" unless is_field && is_owner_field
				
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
				raise "Expected struct for action type #{type}" unless type.ref.is_a?(AST::Struct)
				func = type.ref.actions[ast.action_type]
				func_type, inst_args = inst_ex(ast.source, func, type.args)

				result = func_type.args[Core::Func::Args]

				Result.new(result, false)

			# Values only

			when AST::TypeAssert
				value = analyze_value(ast.value, args.next)
				type = analyze_type(ast.type, args.next)
				unify(value, type)
				Result.new(value, true)
			when AST::ValueTuple
				if args.lvalue
					raise CompileError.new("Unexpected tuple assignment\n#{ast.source.format}") unless args.tuple_lvalue
					Result.new(make_tuple(ast.source, ast.nodes.map { |n| analyze_value(n, args.next(lvalue: true, tuple_lvalue: true)) }), true)
				else
					type = make_tuple(ast.source, ast.nodes.map { |n| analyze_value(n, args.next) })
					ast.gen = type
					Result.new(type, true)
				end
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
					w.cases.each do |c|
						c_type = analyze_value(c, args.next)
						unify(type, c_type)
					end
					analyze_value(w.group, args.next(unused: args.unused))
				end

				cases += [analyze_value(ast.else_group, args.next)] if ast.else_group
				
				cases.inject { |m, o| unify(m, o); m } unless args.unused

				ast.gen = {type: type, unused: args.unused}

				Result.new(args.unused ? unit_type(ast.source) : cases.first, true)
			when AST::MatchAs
				type = analyze_value(ast.expr, args.next)

				@vars[ast.rest.binding] = type

				when_objs = ast.rest.whens.map do |w|
					objs = w.cases.map do |c|
						c_type = analyze_impl(c, args.next)
						obj = case c_type
							when RefResult
								raise "Expected union case\n#{w.source.format}" unless c_type.obj.is_a?(AST::StructCase)
								unify(type, Types::Ref.new(c.source, c_type.obj.parent, c_type.args))
								c_type.obj
							else
								raise "Expected union case\n#{w.source.format}"
						end
					end

					if objs.size == 1
						extended = args.extended.dup
						extended[ast.rest.binding] = objs.first
					end
					
					[objs, analyze_value(w.group, args.next(extended: extended, unused: args.unused))]
				end

				ast.gen = {expr: type, when_objs: when_objs.map(&:first), unused: args.unused}

				cases = when_objs.map(&:last)

				cases += [analyze_value(ast.rest.else_group, args.next)] if ast.rest.else_group

				cases.inject { |m, o| unify(m, o); m } unless args.unused

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
				result = ast.value ? analyze_value(ast.value, args.next) : unit_type(ast.source)
				prev = @result[args.func]
				
				if prev
					unify(result, prev)
					result
				else
					@result[args.func] = result
				end
				
				Result.new(unit_type(ast.source), true)
			when AST::Break
				raise CompileError.new("Break without a loop\n#{ast.source.format}") unless args.loop
				ast.gen = args.loop
				Result.new(unit_type(ast.source), true)
			when AST::Loop
				analyze_value(ast.group, args.next(loop: ast, unused: true))
				Result.new(unit_type(ast.source), true)
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::Ref.new(ast.source, Core::Bool))
				
				l = analyze_value(ast.group, args.next(unused: args.unused))

				if ast.else_node
					r = analyze_value(ast.else_node, args.next(unused: args.unused))
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
					raise CompileError.new("Invalid l-value\n#{ast.source.format}") if args.lvalue

					type, value = coerce_typeval(obj_result, next_args)
					
					result = new_var(ast.source)
					
					
					ast.gen = {args: callable_args, result: result, obj_type: type}
					
					ast.gen[:type] = if value
						# It's a call
						limit = typeclass_limit(ast.source, Core::Callable::Node, {Core::Callable::T => type})
						limit.eq_limit(ast.source, callable_args, Core::Callable::Args)
						limit.eq_limit(ast.source, result, Core::Callable::Result)

						:call
					else

						# It's a constructor
						limit = typeclass_limit(ast.source, Core::Constructor::Node, {Core::Constructor::T => type})
						limit.eq_limit(ast.source, callable_args, Core::Constructor::Args)
						limit.eq_limit(ast.source, result, Core::Constructor::Constructed)

						:construct
					end
					
					req_level(ast.source, result, false) # Constraint on Callable.Result / Types must be sizeable for constructor syntax
					
					Result.new(result, true)
				end
			when AST::Subscript
				obj_type = analyze_value(ast.obj, args.next(lvalue: args.lvalue))
				idx_type = analyze_value(ast.idx, args.next)

				result = new_var(ast.source)
				
				# It's a indexed assignment
				limit = typeclass_limit(ast.source, Core::Indexable::Node, {Core::Indexable::T => obj_type})
				limit.eq_limit(ast.source, idx_type, Core::Indexable::Index)
				limit.eq_limit(ast.source, result, Core::Indexable::Result)

				ast.gen = {result: make_ptr(ast.source, result), obj_type: obj_type}

				Result.new(result, true)
			when AST::Literal
				result = case ast.type
					when :nil
						type = new_var(ast.source)
						Types::Ref.new(ast.source, Core::Option::Node, {Core::Option::T => type})
					when :int
						type = new_var(ast.source)
						typeclass_limit(ast.source, Core::Num::Node, {Core::Num::T => type})
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
				analyze_impl(ast.node, args)
			when AST::UnaryOp
				raise CompileError.new("Invalid l-value\n#{ast.source.format}") if (ast.op != '*') && args.lvalue
				
				case ast.op
					when '+'
						type = analyze_value(ast.node, args.next)
						typeclass_limit(ast.source, Core::Num::Node, {Core::Num::T => lhs}) 
						ast.gen = type
						Result.new(type, true)
					when '*'
						type, value = analyze(ast.node, args.next)
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
						type = analyze_value(ast.node, args.next(lvalue: true))
						result = make_ptr(ast.source, type)
						ast.gen = result
						Result.new(result, true)
					when '!'
						type = analyze_value(ast.node, args.next)
						unify(type, Types::Ref.new(ast.source, Core::Bool))
						ast.gen = type
						Result.new(type, true)
					else
						type = analyze_value(ast.node, args.next)
						map = Core::UnaryOpMap[ast.op]
						raise CompileError.new("Invalid unary operator '#{ast.op}'\n#{ast.source.format}") unless map

						typeclass_limit(ast.source, map[:ref], {map[:param] => type}) 
						ast.gen = type
						Result.new(type, true)
				end
			when AST::BinOp
				assign_op = Parser::AssignOps.include?(ast.op)
				assign_simple = ast.op == '='
				next_args = args.next(lvalue: assign_op, tuple_lvalue: assign_simple)
				
				lresult = analyze_impl(ast.lhs, next_args)
				
				lhs, lvalue = coerce_typeval(lresult, next_args)
				rhs, rvalue = analyze(ast.rhs, args.next)
				
				raise CompileError.new("Left side is #{lvalue ? 'a' : 'of'} type '#{lhs.text}'\n#{ast.lhs.source.format}\nRight side is #{rvalue ? 'a' : 'of'} type '#{rhs.text}'\n#{ast.rhs.source.format}") if lvalue != rvalue
				
				if lvalue
					req_level(ast.source, lhs) if assign_op

					plain_op = assign_op ? (assign_simple ? '=' : ast.op[0]) : ast.op

					typeclass = Core::OpMap[plain_op]

					result = lhs
					if typeclass
						typeclass_limit(ast.source, typeclass[:ref], {typeclass[:param] => lhs}) 

						result = Types::Ref.new(ast.source, typeclass[:result]) if typeclass[:result]

						if typeclass[:rhs]
							unify(rhs, Types::Ref.new(ast.source, typeclass[:rhs]))
						else
							unify(lhs, rhs)
						end
					else
						unify(lhs, rhs)
					end

					unify(lhs, Types::Ref.new(ast.source, Core::Bool)) if ['or', 'and'].include?(plain_op)

					ast.gen = {arg: lhs, rhs: rhs, result: result, plain_op: plain_op}
					Result.new(result, true)
				else
					raise CompileError.new("Unknown type operator '#{ast.op}'\n#{ast.source.format}") if ast.op != '->'
					
					func_args = case ast.lhs
						when AST::Grouped
							arg = analyze_type(ast.lhs.node, args.next)
							make_tuple(ast.lhs.source, [arg])
						when AST::TypeTuple
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
					ast.gen = {type: :var, result: result}
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
					raise CompileError.new("Object doesn't have a scope (#{type})\n#{ast.obj.source.format}") unless type.is_a?(Types::Ref) && type.ref.is_a?(AST::Complex)
					
					ref, ext = scope_lookup(type.ref, ast.name, nil)
					
					raise CompileError.new("'#{ast.name}' is not a member of '#{type.ref.scoped_name}'\n#{ast.source.format}\nScope inferred from:\n#{ast.obj.source.format}") unless ref
					
					ensure_shared(ref, ast.source, args)
					
					RefResult.new(ast, ref, type.args.dup)
				end
				
			when AST::Index
				next_args = args.next(scoped: args.scoped, typeclass: args.typeclass)

				indices = ast.args

				result = analyze_impl(ast.obj, next_args)
				
				case result
					when RefResult, ValueFieldResult
						coerce_typeval_impl(result, next_args, indices)
					else
						raise CompileError.new("[] operator unsupported\n#{ast.source.format}")
				end
			else
				raise "Unknown AST #{ast.class}"
		end
	end
	
	def inst_ex(*args)
		@ctx.inst_ex(*args)
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
						raise CompileError.new(@ctx.recmsg(var, type)) if TypeContext.occurs_in?(var, type)
					else
						raise CompileError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
				end
			end

			next nil unless ref

			raise CompileError.new("Object doesn't have a scope (#{type})\n#{c.ast.source.format}") unless ref.is_a?(AST::Complex)

			field, extension = scope_lookup(ref, c.name, c.extension[:ref])
			
			raise CompileError.new("'#{c.name}' is not a field in '#{ref.scoped_name}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless field
			
			lvalue_check(c.ast.source, field, c.infer_args.lvalue)
		
			map_type_params(c.source, field, parent_args, c.args, c.infer_args)
			
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
			raise(CompileError.new("'#{type.text}' is not a type class\n#{ast_type.source.format}")) if type.fixed_type?
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
		func_args = if func.var_arg
			 	@vars[func.params.first.var]
			else
				make_tuple(func.source, func.params.map { |p| @vars[p.var] })
			end
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

		typeclass_limit(func.params.first.source, Core::Tuple::Node, {Core::Tuple::T => @vars[func.params.first.var]}) if func.var_arg
		
		# TODO: Constraints on actions should apply to the generated typeclass instances too

		if [:create, :create_args].include? func.action_type
			get_field = proc do |name|
				r = owner.scope.names[name]
				next r if r
				owner.parent.scope.names[name] if owner.is_a?(AST::StructCase)
			end
			
			inits = Hash[func.init_list.map { |i| [get_field.(i.field), true] }]

			nodes = owner.scope.names.values
			(nodes = owner.parent.scope.names.values + nodes) if owner.is_a?(AST::StructCase)

			func.gen_init_list = nodes.map do |var|
				case var
					when AST::Variable
						if !var.props[:shared] && !inits[var]
							var_type, inst_args = inst_ex(func.source, var, owner.ctype.type.args)
							typeclass_limit(var.source, Core::Defaultable::Node, {Core::Defaultable::T => var_type})
							{field: var, type: var_type}
						end
				end
			end.compact
		end

		if func.action_type == :copy
			owner.scope.names.values.each do |var|
				case var
					when AST::Variable
						if !var.props[:shared] 
							var_type, inst_args = inst_ex(func.source, var, owner.ctype.type.args)
							typeclass_limit(var.source, Core::Copyable::Node, {Core::Copyable::T => var_type})
						end
				end
			end
		end

		func.init_list.each { |i| analyze_impl(i, a_args) }

		if func.constraints
			# TODO: Process constraints and check that the implementation matches later
		else
			analyze(func.scope, a_args.next(unused: true))
		end
			
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
		raise CompileError, "Unable to find the type of the following expressions in #{@obj.name}\nLimits:\n#{@ctx.limits.map{|i| " - #{i}"}.join("\n")}\nVars:\n#{vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@ctx.var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{@obj.source.format}"
	end

	def assume_parents
		AST.owners(@obj, false).any? do |o|
			infer(o)
			@ctx.assume(o.ctype.ctx.limits)
		end
	end

	def process_constraints(args)
		nil while proc do
			solve_fields or
			@ctx.reduce(@obj, args) or
			assume_parents
		end.()

		@fields.each do |c|
			raise CompileError.new("Unable to find field '#{c.name}' in type '#{c.type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{c.type.source.format}" if c.var.source}")
		end
	end
	
	def finalize(type, value)
		type = type.prune
		@value = value

		@obj.kind_params.ctx.each do |ast|
			n_args = analyze_args.next(scoped: true)
			ctype, value = coerce_typeval(analyze_impl(ast, n_args), n_args)
			raise CompileError, "Expected typeclass\n#{ast.obj.source.format}" unless !value && ctype.ref.is_a?(AST::TypeClass)
		end
		
		# If we are a complex, we can allow to give less strict constraints in case @ctx.reduce refers back to it
		if @obj.is_a? AST::Complex
			@type = type
			@obj.ctype = self
		end

		process_constraints(@obj.is_a?(AST::Complex) ? type.args.values : [])
		
		type_vars = @ctx.type_vars.dup
		
		type_vars.reject! { |var| var.instance }
		
		unresolved_vars = type_vars
		
		case @obj
			# Type variables in functions will be converted to type parameters, we can ignore them
			# Type functions by definition has an unknown type, ignore those too
			when AST::Function, AST::TypeFunction
				type_vars = type_vars.select { |var| TypeContext.occurs_in?(var, type) }
				unresolved_vars -= type_vars
			else
				type_vars = []
		end
		
		# Remove type variables which appear in type function constraints in type class limits
		@dependent_vars, type_vars = @ctx.find_dependent_vars(unresolved_vars, type_vars)
		unresolved_vars -= @dependent_vars
		
		# BROKEN
		# Find unresolved type variables used in type class constraints
		# TODO: This allows variable with just p1 types, find a better way to filter type variables
		#unresolved_vars = @ctx.vars_in_typeclass_args(unresolved_vars)
		
		parameterize(type_vars) if @obj.is_a?(AST::Function) && !@obj.action_type
		
		Silence.puts("") unless @ctx.limits.empty? && @views.empty?
		
		Silence.puts "#{@obj.scoped_name}#{"(#{@obj.type_params.map{|k,v|k.name}.join(",")})" unless @obj.type_params.empty?}  ::  #{type.text}"
		Silence.puts("Instance of #{@obj.typeclass.obj.scoped_name}(#{TypeContext.print_params(@typeclass)})") if @typeclass

		@ctx.limits.each{|i| Silence.puts "    - #{i}"}
		@views.each_pair{|k,v| Silence.puts "    * #{k.text} <= #{v.text}"}
		
		report_unresolved_vars(unresolved_vars) unless unresolved_vars.empty?
		
		@public_ctx = @ctx
		@type = type
		@obj.ctype = self
	end
	
	def process_instance
		process_type_params

		typeclass = @obj.typeclass.obj
		a_args = analyze_args.next(instance: true)
		raise CompileError, "Expected #{typeclass.type_params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.type_params.size
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
						Core.create_enum_str(value)
					when AST::StructCase
						Core.create_constructor_action(value) unless value.actions[:create_args]
						Core.create_constructor(value)
					when AST::Struct
						unless value.enum?
							Core.create_constructor_action(value) unless value.actions[:create_args]
							Core.create_constructor(value) if value.actions[:create_args]
							Core.create_def_constructor(value) if value.sizeable && value.actions[:create]
						end
				end

				if value.is_a?(AST::Struct)
					# TODO: Destroy actions should have the same type context as their parent.
					#       Currently type constraints on them will be ignored as nothing references them.
					Core.create_empty_action(value, :destroy) if !value.actions[:destroy]
					Core.create_empty_action(value, :copy) if value.real_struct.actions[:copy] && !value.actions[:copy]
					Core.create_empty_action(value, :copy) if value.is_a?(AST::Enum)

					Core.create_sizable(value) if value.sizeable
					Core.create_copyable(value) if value.sizeable && value.actions[:copy]
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
			when AST::TypeParamValue
				inst_param = proc { |tp| inst_ex(tp.source, tp, {}).first }
				finalize(coerce_typeparam(value.owner, value.value, analyze_args, inst_param), value.owner.value)
			else
				raise "Unknown value #{value.class}"
		end
	end

	def tci_is_instance(input, inst)
		map = {}
		
		result = Types.cmp_types(input, inst) do |input, inst|
			if inst.is_a?(Types::Variable)
				if map.key? inst
					[true, map[inst] == input]
				elsif input.is_a?(Types::Variable)
					map[inst] = input
					[true, true]
				else
					[true, false]
				end
			else
				[input.is_a?(Types::Variable), false]
			end
		end
		
		[result, map]
	end
	
	def process_all
		raise "process_all before process_req for #{@obj.scoped_name}" unless @obj.ctype

		# Process type parameter values
		@obj.kind_params.values.values.each { |v| InferContext.process(v, @infer_args) }

		case @obj
			when AST::Function
				if @obj.constraints
					@ctx = TypeContext.new(infer_args)
					analyze(@obj.scope, analyze_args.next(unused: true)) 
					process_constraints([])
					unless @ctx.limits.size == 0
						c = @ctx.limits.map{|i| "    - #{i}"}.join("\n")
						raise(CompileError, "Expected no constraints for #{@obj.scoped_name}\n#{@obj.source.format}\n#{c}") 
					end
				end
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

					# TODO: Compare kinds of type parameters too
					raise TypeError, "Wrong type parameter count for '#{name}' in instance of typeclass #{typeclass.scoped_name}, got #{member.type_params.size}, but required #{value.type_params.size}\n#{member.source.format}" unless member.type_params.size == value.type_params.size
					
					param_map = @typeclass.dup
					value.type_params.zip(member.type_params).each do |k,v|
						param_map[k] = infer(v)
					end
					
					ctx = TypeContext.new(@infer_args)
					expected_type = ctx.inst(member.source, value, param_map)
					nil while ctx.reduce(nil)
					
					equal, map = tci_is_instance(m, expected_type)
					#TODO: Check that all limits in member are also limits in value using map to compare type variables

					#TODO: Limits on functions should apply to typeclass instance, else it's unsound
					#      How do deal with higher order functions?
					#          Don't allow type parameters of the function and to mix in a limit
					#      Does the whole typeclass instance need to be a type context?

					raise TypeError, "#{m == expected_type}Expected type '#{expected_type.text}' for '#{name}' in instance of typeclass #{typeclass.scoped_name}, but '#{m.text}' found\n#{member.source.format}\nTypeclass definition\n#{value.source.format}" unless equal
					
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

	def self.infer_core(infer_args)
		infer(Core::Ptr::Node, infer_args)
		infer(Core::Func::Node, infer_args)
		infer(Core::Unit, infer_args)
		infer(Core::Cell::Node, infer_args)
		infer(Core::Option::Node, infer_args)
		infer(Core::Bool, infer_args)
	end

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
