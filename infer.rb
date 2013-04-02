class TypeContext
	attr_accessor :obj, :type, :type_vars, :template_vars, :limits, :typeclass
	
	class TypeError < CompileError
	end
	
	class Limit
		attr_accessor :ast, :var
		
		def initialize(system, ast, var)
			@system = system
			@ast = ast
			@var = var
		end
		
		def name
			@name ||= @system.new_limit_name
		end
	end
	
	class TypeClassLimit < Limit
		attr_accessor :instance
		
		def initialize(system, ast, var)
			@system = system
			@ast = ast
			@var = var
		end
		
		def to_s
			"#{name} := #{@var.text}#{" #{instance.text}" if instance}"
		end
	end
	
	class TypeFunctionLimit < Limit
		attr_accessor :type_ast, :typeclass_limit
		
		def initialize(system, ast, var, type_ast, typeclass_limit)
			super(system, ast, var)
			@type_ast = type_ast
			@typeclass_limit = typeclass_limit
		end
		
		def to_s
			"#{@var.real_text} = #{@typeclass_limit.name}.#{@type_ast.name}"
		end
	end
	
	class FieldLimit < Limit
		attr_accessor :type, :name
		
		def initialize(system, ast, var, type, name)
			super(system, ast, var)
			@type = type
			@name = name
		end
		
		def to_s
			"#{@var.real_text} = #{@type.text}.#{@name}"
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
		@template_vars = {}
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
	
	def make_type_ref(ast, args)
		parent_args = {}
		case ast
			when AST::Ref
				inst_obj = ast.obj
				type = infer(ast.obj)
			when AST::Field
				parent = make_type(ast.source, ast.obj)
				if parent.kind_of? Types::Complex
					ref = parent.complex.scope.require(ast.source, ast.name)
					inst_obj = ref
					parent_args = parent.args
					type = infer(ref)
				else
					raise TypeError.new("Type #{parent.text} doesn't have a scope\n#{ast.source.format}")
				end
			else
				raise TypeError.new("Expected named type reference\n#{ast.source.format}")
		end
		
		ref = ast.obj
		
		if type.kind_of?(Types::Complex)
			args ||= []
			
			if type.type_class?
				type_class_result = new_var(ast.source)
				args.unshift(type_class_result) 
			end
			
			raise TypeError.new("Too many type parameter(s) for #{type.text}, got #{args.size} but maximum is #{type.complex.params.size}\n#{ast.source.format}") if type.complex.params.size < args.size
			
			args.each_with_index do |arg, i|
				parent_args[type.complex.params[i]] = arg
			end
			
			type.complex.params.each do |param|
				parent_args[param] ||= new_var(ast.source)
			end

			result = inst(inst_obj, parent_args, type).source_dup(ast.source)
			
			if type.type_class?
				@limits << TypeClassLimit.new(self, ast, result)
				@views[type_class_result] = result
				result = type_class_result
			end
		elsif args
			raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{ast.source.format}")
		else
			result = inst(inst_obj, parent_args, type).source_dup(ast.source)
		end
		
		result
	end
	
	def make_type(source, ast)
		return new_var(source) unless ast
		
		case ast
			when AST::Function
				Types::Function.new(ast.source, ast.params.map { |p| make_type(ast.source, p) }, make_type(ast.source, ast.result))
			when AST::Function::Param
				make_type(ast.source, ast.type)
			when AST::UnaryOp
				raise TypeError.new("Only pointer (*) unary operator allowed on types\n#{ast.source.format}") if ast.op != :'*'
				Types::Ptr.new(make_type(ast.source, ast.node))
			when AST::FunctionType
				func_args = case ast.arg
					when AST::Grouped
						arg = make_type(ast.arg.source, ast.arg.node)
						Types::Complex.new(ast.arg.source, AST::Cell::Node, {AST::Cell::Val => arg,  AST::Cell::Next => AST::Unit.ctype.type.source_dup(ast.arg.source)})
					when AST::Tuple
						make_type(ast.source, ast.arg)
					else
						make_type(ast.source, ast.arg)
						# TODO: Constraint to tuple type instances only!
						#raise TypeError.new("Only tuple types allowed as arguments to function type constructor (->)\n#{ast.arg.source.format}")
				end				
				
				Types::Function.new(ast.source, func_args, make_type(ast.source, ast.result))
			when AST::Grouped
				make_type(ast.source, ast.node)
			when AST::Index
				make_type_ref(ast.obj, ast.args.map { |n| make_type(ast.source, n) })
			when AST::Ref, AST::Field
				make_type_ref(ast, nil)
			when AST::Variable
				make_type(ast.source, ast.type)
			else
				raise "Unknown AST #{ast.class}"
		end
	end
	
	AnalyzeArgs = Struct.new(:func)
	
	def analyze(ast, args)
		case ast
			when AST::Return
				result = analyze(ast.value, args)
				prev = @result
				
				if prev
					unify(result, prev)
				else
					@result = result
				end
			when AST::If
				cond = analyze(ast.condition, args)
				unify(cond, Types::BoolType)
				
				unit_default analyze(ast.group, args)
				analyze(ast.else_node, args) if ast.else_node
				
				@unit_type
			when AST::BinOp
				lhs = analyze(ast.lhs, args)
				rhs = analyze(ast.rhs, args)
				unify(lhs, rhs)
				lhs
			when AST::Call
				type = analyze(ast.obj, args)
				result = new_var(ast.source)
				call_args = ast.args.map { |arg| analyze(arg, args) }
				
				callable_args = call_args.reverse.inject(AST::Unit.ctype.type.source_dup(ast.source)) { |m, t| Types::Complex.new(ast.source, AST::Cell::Node, {AST::Cell::Val => t, AST::Cell::Next => m}) }
				
				type_class = Types::Complex.new(ast.source, AST::Callable::Node, {AST::Callable::T => type})
				limit = TypeClassLimit.new(self, ast, type_class)
				@limits << limit
				@limits << TypeFunctionLimit.new(self, ast, callable_args, AST::Callable::Args, limit)
				@limits << TypeFunctionLimit.new(self, ast, result, AST::Callable::Result, limit)
				result
			when AST::Field
				type = analyze(ast.obj, args)
				result = new_var(ast.source)
				@limits << FieldLimit.new(self, ast, result, type, ast.name)
				result
			when AST::Ref
				result = @vars[ast.obj]
				if result
					result
				else
					inst(ast.obj)
				end
			when AST::Literal
				case ast.type
					when :int
						AST::Int.ctype.type
					when :bool
						AST::Bool.ctype.type
					when :string
						AST::String.ctype.type
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source)
			when AST::Scope
				if ast.nodes.empty?
					@unit_type
				else
					ast.nodes[0...-1].each do |node|
						unit_default analyze(node, args)
					end
					analyze(ast.nodes.last, args)
				end
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
				args.lmap[limit] ||= TypeClassLimit.new(self, limit.ast, inst_type(args, limit.var))
			when TypeFunctionLimit
				TypeFunctionLimit.new(self, limit.ast, inst_type(args, limit.var), limit.type_ast, inst_limit(args, limit.typeclass_limit))
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
			puts "Comparing #{inst.ctype.typeclass.text} with #{input.text} = #{result}"
			result
		end, map]
	end
	
	def solve
		nil while @limits.reject! do |c|
			case c
				when TypeClassLimit
					unless c.instance
						puts "Searching instance for #{c}"
						inst, map = find_instance(c.var)
						if inst
							c.instance = inst(inst, map)
							puts "Found instance for #{c}"
							false
						elsif c.var.fixed_type?
							raise TypeError.new("Unable to find an instance of the type class '#{c.var.text}'\n#{c.ast.source.format}")
						end
					end
				when TypeFunctionLimit
					inst = c.typeclass_limit.instance
					if inst
						ast = inst.complex.scope.require(c.ast.source, c.type_ast.name)
						
						result = inst(ast, inst.args)
						puts "Found typefunction result for #{c} - #{result.text}"
						
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
							
							field_type = inst(field, type.args)
							
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
		@result = (make_type(func.source, func.result) if func.result)
		
		func.scope.names.each_value do |var|
			@vars[var] = make_type(var.source, var)
		end
		
		func_args = func.params.reverse.inject(AST::Unit.ctype.type.source_dup(func.source)) { |m, p| Types::Complex.new(func.source, AST::Cell::Node, {AST::Cell::Val => @vars[p.var], AST::Cell::Next => m}) }
		
		analyze(func.scope, AnalyzeArgs.new(func))
		
		# TODO: make @result default to Unit, so the function can reference itself
		func_result = Types::Function.new(func.source, func_args, @result || AST::Unit.ctype.type.source_dup(func.source))
		
		solve
		
		finalize(func_result)
		
		unless @func_instances.empty?
			puts "func instances of #{func.name}"
			puts *@func_instances.map{|i| " - #{i}"}
		end
	end
	
	def finalize(type)
		type = type.prune
		
		@type_vars.reject! { |var| var.instance }
		
		unresolved_vars = @type_vars
		@type_vars = @type_vars.select { |var| occurs_in?(var, type) }
		
		@limits.each do |c|
			@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
		end
		
		unresolved_vars -= @type_vars
		unresolved_vars -= @template_vars.values
		
		puts "Type of #{@obj.name} is #{type.text}"
		
		unless @limits.empty?
			puts "  limits:"
			@limits.each{|i| puts "    - #{i}"}
		end
		
		unless @views.empty?
			puts "  views:"
			@views.each_pair{|k,v| puts "    - #{k.text} <= #{v.text}"}
		end
		
		unless @template_vars.empty?
			puts "  template_vars:"
			@template_vars.each_pair{|k,v| puts "    - #{k.name} => #{v.text}"}
		end
		
		unless unresolved_vars.empty?
			raise TypeError, "Unresolved vars of #{@obj.name}\n#{unresolved_vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}\n#{@var_allocs[c][0..3].join("\n")}"}.join("\n")}\n#{obj.source.format}"
		end
		
		@type = type
	end
	
	def process_instance
		typeclass = @obj.typeclass.obj
		raise TypeError, "Expected #{typeclass.params.size} type arguments(s) to typeclass #{typeclass.name}, but #{@obj.args.size} given\n#{@obj.source.format}" if @obj.args.size != typeclass.params.size
		@typeclass = Types::Complex.new(@obj.source, typeclass, Hash[@obj.args.each_with_index.map { |arg, i| [typeclass.params[i], make_type(arg.source, arg)] }])
		puts "typeclass instance #{@typeclass.text}"
		finalize(Types::Complex.new(@obj.source, @obj, Hash[@obj.params.map { |p| [p, Types::Param.new(p.source, p)] }]))
	end
	
	def process_fixed(obj)
		finalize(make_type(@obj.source, @obj))
	end
	
	def process
		value = @obj
		
		case value
			when AST::TypeClassInstance
				process_instance
				TypeContext.infer_scope(value.scope)
			when AST::Complex::Param
				finalize(Types::Param.new(value.source, value))
			when AST::Complex
				parent = value.scope.parent.owner
				if parent.is_a? AST::Complex
					parent = parent.ctype.type.args.each.to_a
				else
					parent = []
				end
				finalize(Types::Complex.new(value.source, value, Hash[value.params.map { |p| [p, Types::Param.new(p.source, p)] } + parent]))
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
				finalize(Types::TypeFunc.new(value.source, value))
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
