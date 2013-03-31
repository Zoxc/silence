class InternalType
	attr_accessor :obj, :type, :type_vars, :template_vars, :limits
	
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
	
	def initialize
		@vars = {}
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
		var
	end
	
	def unit_default(type)
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
						Types::Complex.new(ast.source, AST::Cell, [arg, AST::Unit.itype.type])
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
				type = make_type(ast.source, ast.obj)
				
				if type.kind_of?(Types::Complex) && (type.args.size > 0)
					args = ast.args.map { |n| make_type(ast.source, n) }
					if type.type_class?
						type_class_result = new_var(ast.source)
						args.unshift(type_class_result) 
					end
					raise TypeError.new("Too many type parameter(s) for #{type.text}, got #{type.args.size} but maximum is #{args.size}\n#{ast.source.format}") if type.args.size < args.size
					result = Types::Complex.new(ast.source, type.complex, type.args.size.times.map { |i| args[i] || new_var(ast.source) })
					if type.type_class?
						@limits << TypeClassLimit.new(self, ast, result)
						@views[type_class_result] = result
						type_class_result
					else
						result
					end
				else
					raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{ast.source.format}")
				end
			when AST::Ref
				ref = ast.obj
				
				if ref.kind_of? AST::Complex::Param
					@template_vars[ref] ||= new_var(ref.source)
				else
					InferUtils.infer_type(ref, @infer_args)
					ref.itype.type # remove Complex::Params?
				end
			when AST::Variable
				make_type(ast.source, ast.type)
			else
				raise "Unknown AST #{ast.class}"
		end
	end
	
	class AnalyzeArgs
		attr_accessor :func

		def initialize
			@func = nil
		end
	end
	
	def analyze(ast, args = AnalyzeArgs.new)
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
				
				callable_args = call_args.reverse.inject(AST::Unit.itype.type) { |m, t| Types::Complex.new(ast.source, AST::Cell, [t, m]) }
				
				type_class = Types::Complex.new(ast.source, AST::Callable, [type, callable_args])
				limit = TypeClassLimit.new(self, ast, type_class)
				@limits << limit
				@limits << TypeFunctionLimit.new(self, ast, result, AST::CallableResult, limit)
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
						AST::Int.itype.type
					when :bool
						AST::Bool.itype.type
					when :string
						AST::String.itype.type
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source)
			when AST::Function
				@result = (make_type(ast.source, ast.result) if ast.result)
				new_args = args.dup
				
				ast.scope.names.each_value do |var|
					@vars[var] = make_type(ast.source, var)
				end
				
				new_args.func = ast
				
				analyze(ast.scope, new_args)
				
				func_args = ast.params.reverse.inject(AST::Unit.itype.type) { |m, p| Types::Complex.new(ast.source, AST::Cell, [@vars[p.var], m]) }
				
				type = Types::Function.new(ast.source, func_args, @result || AST::Unit.itype)
				unify(type, @vars[ast])
				type
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
	
	InstArgs = Struct.new(:map, :lmap, :base)
	
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
			when Types::Ptr
				Types::Ptr.new(type.source, inst_type(args, type.type))
			when Types::Function
				Types::Function.new(type.source, inst_type(args, type.args), inst_type(args, type.result))
			when Types::Complex
				Types::Complex.new(type.source, type.complex, type.args.map { |arg| inst_type(args, arg) })
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst(obj, base = nil)
		InferUtils.infer_type(obj, @infer_args)
		itype = obj.itype
		map = {}
		inst_args = InstArgs.new(map, {}, base)
		
		itype.template_vars.each_pair do |param, type|
			var = inst_type(inst_args, type)
			
			other = if base
				index = base.complex.params.index(param)
				base.args[index]
			else
				raise "Outside scope of type param" unless @obj.scope.inside?(param.owner.scope)
				@template_vars[param] ||= new_var(var.source)
			end
			
			unify(var, other)
		end
		
		vars = itype.type_vars.map { |tv| map[tv] ||= new_var(tv.source) }
		
		@limits.concat(itype.limits.map { |l| inst_limit(inst_args, l) })
		
		return inst_type(inst_args, itype.type)
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
	
	def solve
		nil while @limits.reject! do |c|
			case c
				when TypeClassLimit
					unless c.instance
						puts "Searching instance for #{c}"
						inst = TypeClass.find_instance(@infer_args, c.var)
						if inst
							typeclass_args = c.var.complex.params.map { new_var(c.ast.source) }
							c.instance = inst(inst, Types::Complex.new(c.ast.source, inst, typeclass_args))
							unify(c.var, Types::Complex.new(c.ast.source, c.var.complex, c.instance.args))
							puts "Found instance for #{c} - #{inst.itype.type.text}"
							false
						elsif c.var.fixed_type?
							raise TypeError.new("Unable to find an instance of the type class '#{c.var.text}'\n#{c.ast.source.format}")
						end
					end
				when TypeFunctionLimit
					inst = c.typeclass_limit.instance
					if inst
						#ast = inst.scope.require(c.ast.source, c.type_ast.name)
						
						#unify(inst(ast, inst), c.var)
						
						#true
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
							
							field_type = inst(field, type)
							
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
	
	def self.create_function_instance(func, map)
		system = InferSystem.new
		system.create_function_instance(func, map)
	end
	
	def create_function_instance(func, map)
		full_map = map.dup
		type = inst_type(full_map, func.itype)
		solve
		puts "map of #{func.name} #{full_map.each.map{|v| "#{v.first.text} => #{v.last.text}" }.join(", ")} with type #{type.text}"
		AST::Function::Instance.new(func, map, full_map, type)
	end
	
	def infer_function(func, infer_args)
		@infer_args = infer_args
		@obj = func
		@type = @vars[func] = new_var(func.source)
		analyze(func)
		
		solve
		
		finalize
		
		unless @func_instances.empty?
			puts "func instances of #{func.name}"
			puts *@func_instances.map{|i| " - #{i}"}
		end
		
		@infer_args = nil
		self
	end
	
	def infer_fixed(obj, infer_args)
		@infer_args = infer_args
		@obj = obj
		@type = make_type(obj.source, obj)
		finalize
		self
	end
	
	def wrap(obj, type)
		@obj = obj
		@type = type
		finalize
		self
	end
	
	def infer_instance(obj, infer_args)
		@infer_args = infer_args
		@obj = obj
		@type = Types::Complex.new(obj.source, obj, obj.args.map { |arg| make_type(arg.source, arg) })
		finalize
		self
	end
	
	def finalize
		@type = @type.prune
		
		@type_vars.reject! { |var| var.instance }
		
		unresolved_vars = @type_vars
		@type_vars = @type_vars.select { |var| occurs_in?(var, @type) }
		
		@limits.each do |c|
			@type_vars.delete(c.var.prune) if c.is_a? FieldLimit
		end
		
		unresolved_vars -= @type_vars
		unresolved_vars -= @template_vars.values
		
		puts "Type of #{@obj.name} is #{@type.text}"
		
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
			raise TypeError, "Unresolved vars of #{@obj.name}\n#{unresolved_vars.map{|c| " - #{c.text}#{"\n#{c.source.format}" if c.source}"}.join("\n")}\n#{obj.source.format}"
		end
		
		@obj.itype = self
	end
end

def get_type(source, ast, args)
	InternalType.new.infer_fixed(ast, args)
end

InferArgs = Struct.new :visited

module InferUtils
	def self.infer_type(value, args)
		return if value.itype
		raise "Recursive #{value.name}" if args.visited[value]
		args.visited[value] = true
		
		case value
			when AST::TypeClassInstance
				value.itype = InternalType.new.infer_instance(value, args)
				infer_scope(value.scope, args)
			when AST::Complex::Param
				value.itype = InternalType.new.wrap(value, Types::Param.new(value.source, value))
			when AST::Complex
				value.itype = InternalType.new.wrap(value, Types::Complex.new(value.source, value, value.params.map { |p| Types::Param.new(p.source, p) }))
				infer_scope(value.scope, args)
			when AST::Variable
				value.itype = get_type(value.source, value, args)
			when AST::Function
				if value.scope
					InternalType.new.infer_function(value, args)
				else
					value.itype = get_type(value.source, value, args)
				end
			when AST::TypeFunction
				value.itype = InternalType.new.wrap(value, Types::TypeFunc.new(value.source, value))
			else
				raise "Unknown value #{value.class}"
		end
	end

	def self.infer_scope(scope, args = InferArgs.new({}))
		scope.names.each_pair do |name, value|
			infer_type(value, args)
		end
	end
end
