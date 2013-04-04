class TypeContext
	attr_accessor :obj, :type, :type_vars, :limits, :typeclass, :value
	
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
		attr_accessor :type, :name, :args
		
		def initialize(system, source, var, type, name, args)
			super(system, source, var)
			@type = type
			@name = name
			@args = args
		end
		
		def to_s
			"#{@var.real_text} = #{@type.text}.#{@name} [#{args.map{ |t|t .text }.join(", ")}]"
		end
	end
	
	FuncInst = Struct.new(:func, :vars) do
		def to_s
			"#{func.name}, {#{vars.size.times.map{|i| "#{func.type_vars[i].text} => #{vars[i].text}" }.join(", ")}}"
		end
	end
	
	def initialize(infer_args, obj)
		@infer_args = infer_args
		@obj = obj
		obj.ctype = self
		@vars = {}
		@var_allocs = {}
		@type_vars = []
		@var_name = 1
		@limit_name = 1
		@func_instances = []
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
	
	def new_var(source = nil)
		var = Types::Variable.new(source, self, nil)
		#puts "new var #{var.text}\n#{source.format}"
		#puts "\n#{caller.join("\n")}"
		@type_vars << var
		@var_allocs[var] = caller
		var
	end
	
	def unit_default(type)
	end
	
	def make_tuple(source, args)
		args.reverse.inject(AST::Unit.ctype.type.source_dup(source)) { |m, p| Types::Complex.new(source, AST::Cell::Node, {AST::Cell::Val => p, AST::Cell::Next => m}) }
	end
	
	class AnalyzeArgs
		attr_accessor :scoped, :index
		def initialize
			@scoped = false
			@index = {}
		end
		
		def next(opts = {})
			new = dup
			new.scoped = opts[:scoped] || @scoped
			new.index = opts[:index] || {}
			new
		end
	end
	
	def map_type_params(source, parent_args, params, args, desc)
		raise TypeError.new("Too many type parameter(s) for #{desc}, got #{args.size} but maximum is #{params.size}\n#{source.format}") if params.size < args.size
		
		args.each_with_index do |arg, i|
			parent_args[params[i]] = arg
		end
		
		params.each do |param|
			parent_args[param] ||= new_var(source)
		end
	end
	
	def analyze_ref(source, inst_obj, direct, parent_args, args, type)
		args[:used] = true
		args = args[:args]
		
		if type.kind_of?(Types::Complex)
			args ||= []
			
			if direct && type.type_class?
				type_class_result = new_var(source)
				args.unshift(type_class_result)
			end
			
			map_type_params(source, parent_args, type.complex.params, args, type.text)
			
			result = inst(inst_obj, parent_args, type).source_dup(source)
			
			if type.type_class?
				@limits << TypeClassLimit.new(self, source, result)
				if direct
					@views[type_class_result] = result 
					result = type_class_result
				end
			end
		elsif args
			raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{source.format}")
		else
			result = inst(inst_obj, parent_args, type).source_dup(source)
		end
		
		[result, false]
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
	
	def get_index_args(args)
		if args.index[:args]
			args.index[:used] = true
			args.index[:args]
		else
			[]
		end
	end
	
	def analyze(ast, args)
		case ast
			# Types only
			
			when AST::Function::Param, AST::Variable
				if ast.type
					[analyze_type(ast.type, args.next), true]
				else
					[new_var(ast.source), true]
				end
			when AST::UnaryOp
				raise TypeError.new("Only pointer (*) unary operator allowed on types\n#{ast.source.format}") if ast.op != :'*'
				[Types::Ptr.new(ast.source, analyze_type(ast.node, args.next)), false]
			when AST::Tuple
				[make_tuple(ast.source, ast.nodes.map { |n| analyze_type(n, args.next) }), false]
			when AST::FunctionType
				func_args = case ast.arg
					when AST::Grouped
						arg = analyze_type(ast.arg.node, args.next)
						make_tuple(ast.arg.source, [arg])
					when AST::Tuple
						analyze_type(ast.arg, args.next)
					else
						analyze_type(ast.arg, args.next)
						# TODO: Constraint to tuple type instances only!
						#raise TypeError.new("Only tuple types allowed as arguments to function type constructor (->)\n#{ast.arg.source.format}")
				end				
				
				[Types::Function.new(ast.source, func_args, analyze_type(ast.result, args.next)), false]
			when AST::Grouped
				analyze(ast.node, args.next)
			
			# Values only
			
			when AST::Return
				result = analyze_value(ast.value, args.next)
				prev = @result
				
				[if prev
					unify(result, prev)
				else
					@result = result
				end, true]
			when AST::If
				cond = analyze_value(ast.condition, args.next)
				unify(cond, Types::BoolType)
				
				unit_default analyze_value(ast.group, args.next)
				analyze_value(ast.else_node, args.next) if ast.else_node
				
				[@unit_type, true]
			when AST::BinOp
				lhs = analyze_value(ast.lhs, args.next)
				rhs = analyze_value(ast.rhs, args.next)
				unify(lhs, rhs)
				[lhs, true]
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
						AST::String.ctype.type
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source), true]
			when AST::Scope
				[if ast.nodes.empty?
					@unit_type
				else
					ast.nodes[0...-1].each do |node|
						unit_default analyze_value(node, args.next)
					end
					analyze_value(ast.nodes.last, args.next)
				end, true]
				
			# The tricky mix
			
			when AST::Ref
				result = @vars[ast.obj]
				if result
					[result, true]
				else
					type = infer(ast.obj)
					if ast.obj.ctype.value
						parent_args = {}
						
						if ast.obj.is_a?(AST::Function)
							map_type_params(ast.source, parent_args, ast.obj.type_params, get_index_args(args), ast.obj.name)
						end
						
						[inst(ast.obj, parent_args), true]
					else
						analyze_ref(ast.source, ast.obj, !args.scoped, {}, args.index, type)
					end
				end
			when AST::Field
				type, value = analyze(ast.obj, args.next(scoped: true))
				
				if value
					result = new_var(ast.source)
					@limits << FieldLimit.new(self, ast.source, result, type, ast.name, get_index_args(args))
					[result, true]
				else
					if type.kind_of? Types::Complex
						ref = type.complex.scope.require(ast.source, ast.name, proc { "Can't find '#{ast.name}' in scope '#{type.text}'" })
						analyze_ref(ast.source, ref, !args.scoped, type.args, args.index, infer(ref))
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
	
	InstArgs = Struct.new(:map, :lmap, :params)
	
	def inst_limit(args, limit)
		case limit
			when TypeClassLimit
				args.lmap[limit] ||= TypeClassLimit.new(self, limit.source, inst_type(args, limit.var))
			when TypeFunctionLimit
				TypeFunctionLimit.new(self, limit.source, inst_type(args, limit.var), limit.type_ast, inst_limit(args, limit.typeclass_limit))
			else
				raise "Unknown limit #{limit.class}"
		end
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Variable
				args.map[type] ||= new_var(type.source)
			when Types::Param
				args.params[type.param] || type
			when Types::TypeFunc
				limit = @limits.find { |l| l.is_a?(TypeClassLimit) && l.var.complex == type.func.declared.owner && l.var.args == args.params }
				raise "Unable to find limit for #{type.text}" unless limit
				result = new_var(type.source)
				@limits << TypeFunctionLimit.new(self, type.func.source, result, type.func, limit)
				result
			when Types::Ptr
				Types::Ptr.new(type.source, inst_type(args, type.type))
			when Types::Function
				Types::Function.new(type.source, inst_type(args, type.args), inst_type(args, type.result))
			when Types::Complex
				Types::Complex.new(type.source, type.complex, Hash[type.args.map { |k, v| [k, inst_type(args, v)] }])
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst(obj, params = {}, type_obj = nil)
		infer(obj)
		map = {}
		inst_args = InstArgs.new(map, {}, params)
		
		@limits.concat(obj.ctype.limits.map { |l| inst_limit(inst_args, l) })
		
		return inst_type(inst_args, type_obj ? type_obj : obj.ctype.type)
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
		
			msg = if a.source && b.source
				"When unifying types '#{a.text}',\n#{a.source.format}\nand type '#{b.text}',\n#{b.source.format}" 
			elsif source
				"When unifying types '#{a.text}' and type '#{b.text}''\n#{source.format}" 
			else
				"When unifying types '#{a.text}' and type '#{b.text}'" 
			end
			
			msg << ("\n" + loc.())
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
						ast = inst.complex.scope.require(c.source, c.type_ast.name)
						
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
							
							if field.is_a?(AST::Function)
								parent_args = type.args.dup
								map_type_params(c.source, parent_args, field.type_params, c.args, field.name)
							else
								parent_args = type.args
							end
							
							field_type = inst(field, parent_args)
							
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
			@vars[var] = analyze_value(var, analyze_args)
		end
		
		func_args = make_tuple(func.source, func.params.map { |p| @vars[p.var] })
		
		analyze(func.scope, AnalyzeArgs.new)
		
		# TODO: make @result default to Unit, so the function can reference itself
		func_result = Types::Function.new(func.source, func_args, @result || AST::Unit.ctype.type.source_dup(func.source))
		
		finalize(func_result, true)
		
		unless @func_instances.empty?
			puts "func instances of #{func.name}"
			puts *@func_instances.map{|i| " - #{i}"}
		end
	end
	
	def finalize(type, value)
		type = type.prune
		@value = value
		solve
		
		@type_vars.reject! { |var| var.instance }
		
		unresolved_vars = @type_vars
		@type_vars = @type_vars.select { |var| occurs_in?(var, type) }
		
		@limits.each do |c|
			@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
		end
		
		unresolved_vars -= @type_vars
		
		@limits.each do |c|
			unresolved_vars.delete(c.var.prune) if c.is_a? TypeFunctionLimit
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
	end
	
	def process_instance
		typeclass = @obj.typeclass.obj
		analyze_args = AnalyzeArgs.new
		raise TypeError, "Expected #{typeclass.params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.params.size
		@typeclass = Types::Complex.new(@obj.source, typeclass, Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.params[i], analyze_type(arg, analyze_args)] }])
		finalize(Types::Complex.new(@obj.source, @obj, Hash[@obj.params.map { |p| [p, Types::Param.new(p.source, p)] }]), false)
		
		TypeContext.infer_scope(@obj.scope)
		
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
				finalize(Types::Complex.new(value.source, value, Hash[value.params.map { |p| [p, Types::Param.new(p.source, p)] } + parent]), false)
				TypeContext.infer_scope(value.scope)
			when AST::Variable
				process_fixed(value)
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

	def self.infer(value, infer_args = InferArgs.new({}))
		return value.ctype.type if value.ctype
		raise "Recursive #{value.name}" if infer_args.visited[value]
		infer_args.visited[value] = true
		
		TypeContext.new(infer_args, value).process
		
		value.ctype.type
	end

	InferArgs = Struct.new :visited

	def self.infer_scope(scope)
		scope.names.each_pair do |name, value|
			infer(value)
		end
	end
end
