class TypeContext
	attr_accessor :obj, :type, :type_vars, :limits, :typeclass, :value, :vars, :infer_args, :type_func_vars
	
	class TypeError < CompileError
	end
	
	class Limit
		attr_accessor :source, :var
		
		def initialize(system, source, var)
			@system = system
			@source = source
			@var = var
		end
		
		def name
			@name ||= @system.new_limit_name
		end
	end
	
	class TypeClassLimit < Limit
		attr_accessor :instance
		
		def to_s
			"#{name} := #{@var.text}#{" #{instance.text}" if instance}"
		end
	end
	
	class TypeFunctionLimit < Limit
		attr_accessor :type_ast, :typeclass_limit
		
		def initialize(system, source, var, type_ast, typeclass_limit)
			super(system, source, var)
			@type_ast = type_ast
			@typeclass_limit = typeclass_limit
		end
		
		def to_s
			"#{@var.real_text} = #{@typeclass_limit.name}.#{@type_ast.name}"
		end
	end
	
	class FieldLimit < Limit
		attr_accessor :type, :name, :args, :ast, :lvalue
		
		def initialize(system, source, var, type, name, args, ast, lvalue)
			super(system, source, var)
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
		@limit_name = 1
		@limits = []
		@views = {}
	end
	
	def new_var_name
		result = @var_name
		@var_name += 1
		"p#{result}"
	end
	
	def new_limit_name
		result = @limit_name
		@limit_name += 1
		"l#{result}"
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
		args.reverse.inject(AST::Unit.ctype.type.source_dup(source)) { |m, p| Types::Complex.new(source, AST::Cell::Node, {AST::Cell::Val => p, AST::Cell::Next => m}) }
	end
	
	def make_ptr(source, type)
		Types::Complex.new(source, AST::Ptr::Node, {AST::Ptr::Type => type})
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
				@limits << TypeClassLimit.new(self, source, result)
				if !analyze_args.scoped
					@views[type_class_result] = result # TODO: Views won't work nice since type_class_result can be unified with anything
					result = type_class_result
				end
			end
		elsif args
			raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{source.format}")
		elsif obj.kind_of?(AST::TypeFunction)
			result, inst_args = inst_ex(obj, parent_args, type)
			result = result.source_dup(source)
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
				
				[AST::Unit.ctype.type.source_dup(ast.source), true]
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
				
				type_class = Types::Complex.new(ast.source, AST::Callable::Node, {AST::Callable::T => type})
				limit = TypeClassLimit.new(self, ast.source, type_class)
				@limits << limit
				@limits << TypeFunctionLimit.new(self, ast.source, callable_args, AST::Callable::Args, limit)
				@limits << TypeFunctionLimit.new(self, ast.source, result, AST::Callable::Result, limit)
				[result, true]
			when AST::Literal
				[case ast.type
					when :int
						AST::Int.ctype.type
					when :bool
						AST::Bool.ctype.type
					when :string
						Types::Complex.new(ast.source, AST::Ptr::Node, {AST::Ptr::Type => AST::Char.ctype.type})
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source), true]
			when AST::Scope
				[if ast.nodes.empty?
					AST::Unit.ctype.type.source_dup(ast.source)
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
							lhs
							# TODO: Constraint to tuple type instances only!
							#raise TypeError.new("Only tuple types allowed as arguments to function type constructor (->)\n#{ast.arg.source.format}")
					end				
					
					[Types::Complex.new(ast.source, AST::Func::Node, {AST::Func::Args => func_args, AST::Func::Result => rhs}), false]
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
					@limits << FieldLimit.new(self, ast.source, result, type, ast.name, get_index_args(args), ast, args.lvalue)
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
	
	def inst_limit(lmap, args, limit)
		case limit
			when TypeClassLimit
				lmap[limit] ||= TypeClassLimit.new(self, limit.source, inst_type(args, limit.var))
			when TypeFunctionLimit
				unless limit.var.is_a? Types::Variable
					TypeFunctionLimit.new(self, limit.source, inst_type(args, limit.var), limit.type_ast, inst_limit(lmap, args, limit.typeclass_limit))
				end
			else
				raise "Unknown limit #{limit.class}"
		end
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Param
				args.params[type.param] || type
			when Types::TypeFunc
				limit = @limits.find { |l| l.is_a?(TypeClassLimit) && l.var.complex == type.func.declared.owner && l.var.args == args.params }
				raise "Unable to find limit for #{type.text}" unless limit
				result = new_var(type.source)
				@limits << TypeFunctionLimit.new(self, type.func.source, result, type.func, limit)
				result
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
		lmap = {}
		inst_args = InstArgs.new(params)
		
		@limits.concat(obj.ctype.limits.map { |l| inst_limit(lmap, inst_args, l) }.compact)
		
		return inst_type(inst_args, type_obj ? type_obj : obj.ctype.type), inst_args
	end
	
	def unify(a, b, loc = proc { "" })
		a = a.prune
		b = b.prune
		
		if a.is_a? Types::Variable
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
	
	def find_instance(input)
		map = nil
		[input.complex.instances.find do |inst|
			infer(inst)
			result, map = TypeClass.is_instance?(@infer_args, input, inst.ctype.typeclass)
			#puts "Comparing #{inst.ctype.typeclass.text} with #{input.text} = #{result}"
			result
		end, map]
	end
	
	def solve
		nil while @limits.reject! do |c|
			case c
				when TypeClassLimit
					unless c.instance
						#puts "Searching instance for #{c}"
						inst, map = find_instance(c.var)
						if inst
							c.instance = inst(inst, map)
							#puts "Found instance for #{c}"
							true
						elsif c.var.fixed_type?
							raise TypeError.new("Unable to find an instance of the type class '#{c.var.text}'\n#{c.source.format}")
						end
					end
				when TypeFunctionLimit
					inst = c.typeclass_limit.instance
					if inst
						ast = inst.complex.scope.names[c.type_ast.name]
						
						result = inst(ast, inst.args)
						
						unify(result, c.var)
						
						true
					end
				when FieldLimit
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
				else
					raise "Unknown limit #{c.class}"
			end
		end
	end
	
	def process_function
		func = @obj
		
		analyze_args = AnalyzeArgs.new
		
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
		func_result = Types::Complex.new(func.source, AST::Func::Node, {AST::Func::Args => func_args, AST::Func::Result => (@result || AST::Unit.ctype.type.source_dup(func.source))})

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
	
	def finalize(type, value)
		type = type.prune
		@value = value
		solve
		
		@type_vars.reject! { |var| var.instance }
		
		unresolved_vars = @type_vars
		
		if @obj.is_a? AST::Function
		
			@type_vars = @type_vars.select { |var| occurs_in?(var, type) }
			
			parameterize(@type_vars)
			
			#@limits.each do |c|
			#	@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
			#end
			
			unresolved_vars -= @type_vars
			
			@type_func_vars = @limits.select do |c|
				next unless c.is_a? TypeFunctionLimit
				var = c.var.prune
				next unless unresolved_vars.include?(var)
				unresolved_vars.delete(var)
				true
			end
		end
		
		puts "Type of #{@obj.name} is #{type.text}"
		
		unless @limits.empty?
			puts "  limits:"
			@limits.each{|i| puts "    - #{i}"}
		end
		
		unless @views.empty?
			puts "  views:"
			@views.each_pair{|k,v| puts "    - #{k.text} <= #{v.text}"}
		end
		
		unless unresolved_vars.empty?
			raise TypeError, "Unresolved vars of #{@obj.name}\n#{unresolved_vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{obj.source.format}"
		end
		
		@type = type
		@obj.ctype = self
	end
	
	def process_instance
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
			b = inst(value, @typeclass.args)
			raise TypeError, "Expected type '#{b.text}' for #{name}, but '#{m.text}' found\n#{member.source.format}" unless m == b
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
		raise "Recursive #{value.name}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		TypeContext.new(infer_args, value).process
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited

	def self.infer_scope(scope, infer_args)
		scope.names.each_pair do |name, value|
			infer(value, infer_args)
		end
	end
end
