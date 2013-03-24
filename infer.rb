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
		attr_accessor :type
		
		def to_s
			"#{name} := #{@var.text}"
		end
	end
	
	class TypeFunctionLimit < Limit
		attr_accessor :type_ast, :interface
		
		def initialize(system, ast, var, type_ast, interface)
			super(system, ast, var)
			@type_ast = type_ast
			@interface = interface
		end
		
		def to_s
			"#{@var.real_text} = #{@interface.name}.#{@type_ast.name}"
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
	
	def make_type(source, scope, ast)
		return new_var(source) unless ast
		
		case ast
			when AST::Function
				Types::Function.new(ast.source, ast.params.map { |p| make_type(ast.source, scope, p) }, make_type(ast.source, scope, ast.result))
			when AST::Function::Param
				make_type(ast.source, scope, ast.type)
			when AST::UnaryOp
				raise TypeError.new("Only pointer (*) unary operator allowed on types\n#{ast.source.format}") if ast.op != :'*'
				Types::Ptr.new(make_type(ast.source, scope, ast.node))
			when AST::FunctionType
				func_args = case ast.arg
					when AST::Grouped
						arg = make_type(ast.arg.source, scope, ast.arg.node)
						Types::Complex.new(ast.source, AST::Cell, [arg, AST::Unit.itype.type])
					when AST::Tuple
						make_type(ast.source, scope, ast.arg)
					else
						raise TypeError.new("Only tuples type allowed as arguments to function type constructor (->)\n#{ast.arg.source.format}")
				end				
				
				Types::Function.new(ast.source, func_args, make_type(ast.source, scope, ast.result))
			when AST::Grouped
				make_type(ast.source, scope, ast.node)
			when AST::Index
				type = make_type(ast.source, scope, ast.obj)
				
				if type.kind_of?(Types::Complex) && (type.args.size > 0)
					args = ast.args.map { |n| make_type(ast.source, scope, n) }
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
					InferUtils.infer_type(ref, ref.declared, @infer_args)
					ref.itype.type # remove Complex::Params?
				end
			when AST::Variable
				make_type(ast.source, scope, ast.type)
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
				@result = (make_type(ast.source, @obj.scope, ast.result) if ast.result)
				new_args = args.dup
				
				ast.scope.names.each_value do |var|
					@vars[var] = make_type(ast.source, @obj.scope, var)
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
				TypeFunctionLimit.new(self, limit.ast, inst_type(args, limit.var), limit.type_ast, inst_limit(args, limit.interface))
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
		case obj
			when AST::Function, AST::Variable
				InferUtils.infer_type(obj, @obj.scope, @infer_args)
			else
				raise "Undefined #{print_ast(obj)}"
		end
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
		
		a_args = a.args
		b_args = b.args
		
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
				when TypeFunctionLimit
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
	
	def infer_fixed(obj, scope, infer_args)
		@infer_args = infer_args
		@obj = obj
		@type = make_type(obj.source, scope, obj)
		finalize
		self
	end
	
	def wrap(obj, type)
		@obj = obj
		@type = type
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
		
		unless @limits.empty?
			puts "limits of #{@obj.name}"
			@limits.each{|i| puts " - #{i}"}
		end
		
		unless @views.empty?
			puts "views of #{@obj.name}"
			@views.each_pair{|k,v| puts " - #{k.text} <= #{v.text}"}
		end
		
		unless @template_vars.empty?
			puts "template_vars of #{@obj.name}"
			@template_vars.each_pair{|k,v| puts " - #{k.name} => #{v.text}"}
		end
		
		puts "Type of #{@obj.name} is #{@type.text}"
		
		unless unresolved_vars.empty?
			raise TypeError, "Unresolved vars of #{@obj.name}\n#{unresolved_vars.map{|c| " - #{c.text}"}.join("\n")}\n#{obj.source.format}"
		end
		
		@obj.itype = self
	end
end

def get_type(source, scope, ast, args)
	InternalType.new.infer_fixed(ast, scope, args)
end

InferArgs = Struct.new :visited

module InferUtils
	def self.done?(ast, args)
		return true if ast.itype
		raise "Recursive #{ast.name}" if args.visited[ast]
		args.visited[ast] = true
		false
	end
	
	def self.infer_function(func, args)
		return if done? func, args
		
		if func.scope
			InternalType.new.infer_function(func, args)
		else
			func.itype = get_type(func.source, func.parent_scope, func, args)
		end
	end

	def self.infer_var(var, scope, args)
		return if done? var, args
		
		var.itype = get_type(var.source, scope, var, args)
	end

	def self.infer_complex(complex, args)
		return if done? complex, args
		
		complex.itype = InternalType.new.wrap(complex, Types::Complex.new(complex.source, complex, complex.params.map { |p| Types::Param.new(p.source, p) }))
		
		infer_scope(complex.scope, args)
	end

	def self.infer_type_function(func, args)
		return if done? func, args
		
		func.itype = InternalType.new.wrap(func, Types::TypeFunc.new(func.source, func))
	end

	def self.infer_type(value, scope, args)
		case value
			when AST::Complex
				infer_complex(value, args)
			when AST::Variable
				infer_var(value, scope, args)
			when AST::Function
				infer_function(value, args)
			when AST::TypeFunction
				infer_type_function(value, args)
			else
				raise "Unknown value #{value.class}"
		end
	end

	def self.infer_scope(scope, args = InferArgs.new({}))
		scope.names.each_pair do |name, value|
			next if value.kind_of? AST::Complex::Param
			infer_type(value, scope, args)
		end
	end
end
