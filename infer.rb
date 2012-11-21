class InferSystem
	class TypeError < CompileError
	end
	
	class Constraint
		attr_accessor :ast, :var
		
		def initialize(ast, var)
			@ast = ast
			@var = var
		end
	end
	
	class ApplyConstraint < Constraint
		attr_accessor :result, :type, :args
		
		def initialize(ast, var, type, args)
			super(ast, var)
			@type = type
			@args = args
		end
		
		def to_s
			"#{@var.real_text} = Apply{ #{@type.text} (#{@args.map(&:text).join(", ")}) }"
		end
	end
	
	class FieldConstraint < Constraint
		attr_accessor :result, :type, :name
		
		def initialize(ast, var, type, name)
			super(ast, var)
			@type = type
			@name = name
		end
		
		def to_s
			"#{@var.real_text} = Field{ #{@type.text} .#{@name} }"
		end
	end
	
	class FuncInstConstraint < Constraint
		attr_accessor :func, :map
		
		def initialize(ast, var, map)
			super(ast, var)
			@map = map
		end
		
		def to_s
			"#{@var.real_text} = FuncInst{ #{@ast.name}, #{@map.each.map{|v| "#{v.first.text} => #{v.last.text}" }.join(", ")} }"
		end
	end
	
	def initialize
		@vars = {}
		@type_vars = []
		@var_name = 1
		@constraints = []
		@func_instances = []
		@make_type_args = MakeTypeArgs.new(proc { |s| new_var(s) }, proc { |ast, type, args|
			raise TypeError.new("Too many type parameter(s) for #{type.text}, got #{type.arg_count} but maximum is #{args.size}\n#{ast.source.format}") if type.arg_count < args.size
			template_args = type.arg_count.times.map { |i| args[i] || new_var(ast.source) }
			type.template_dup(template_args)
		})
	end
	
	def new_var_name
		result = @var_name
		@var_name += 1
		"p#{result}"
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
	
	class MakeTypeArgs
		attr_accessor :new_var, :template_instance

		def initialize(new_var, template_instance)
			@new_var = new_var
			@template_instance = template_instance
		end
	end
	
	def self.make_type(source, scope, ast, args)
		return args.new_var.(source) unless ast
		case ast
			when AST::Function
				Types::Function.new(ast.source, ast.params.map { |p| make_type(ast.source, scope, p, args) }, make_type(ast.source, scope, ast.result, args))
			when AST::Function::Parameter
				make_type(ast.source, scope, ast.type, args)
			when AST::NullPtrTypeNode, AST::PtrTypeNode
				make_type(ast.source, scope, ast.target, args)
			when AST::NamedTypeNode
				type = scope.require(ast.source, ast.name, Types::Type)
				
				if type.template?
					args.template_instance.(ast, type, ast.args.map { |n| make_type(ast.source, scope, n, args) })
				else
					raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{ast.source.format}") unless type.args.empty?
					type
				end
			when AST::Variable
				make_type(ast.source, scope, ast.type, args)
			else
				raise "Unknown AST #{ast.class}"
		end
	end
	
	def make_type(source, args, ast)
		InferSystem.make_type(source, @func.scope, ast, @make_type_args)
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
				@constraints << ApplyConstraint.new(ast, result, type, ast.args.map { |arg| analyze(arg, args) })
				result
			when AST::Field
				type = analyze(ast.obj, args)
				result = new_var(ast.source)
				@constraints << FieldConstraint.new(ast, result, type, ast.name)
				result
			when AST::Ref
				result = @vars[ast.obj]
				if result
					result
				else
					case ast.obj
						when AST::Function
							inst(ast.obj)
						else
							raise "Undefined #{print_ast(ast.obj)}"
					end
				end
			when AST::Literal
				case ast.type
					when :int
						Types::IntType
					when :bool
						Types::BoolType
					when :string
						Types::StringType
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source)
			when AST::Function
				@result = (make_type(ast.source, args, ast.result) if ast.result)
				
				new_args = args.dup
				
				ast.scope.names.each_value do |var|
					@vars[var] = make_type(ast.source, args, var.type)
				end
				
				new_args.func = ast
				
				analyze(ast.scope, new_args)
				
				type = Types::Function.new(ast.source, ast.params.map { |p| @vars[p.var] }, @result || Types::UnitType).source_dup(ast.source)
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
		
		return b.args.any? do |arg|
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
	
	InstArgs = Struct.new(:map, :base)
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Fixed
				type
			when Types::TemplateParam
				index = args.base.struct.template_params.index(type.param)
				args.base.args[index]
			when Types::Variable
				args.map[type] ||= new_var(type.source)
			when Types::Function
				type.args_dup(*type.args.map { |arg| inst_type(args, arg) })
			else
				raise "Unknown type #{type.class}"
		end
	end
	
	def inst_constraint(args, c)
		case c
			when ApplyConstraint
				c.class.new(c.ast, inst_type(args, c.var), inst_type(args, c.type), c.args.map { |arg| inst_type(args, arg) })
			when FieldConstraint
				FieldConstraint.new(c.ast, inst_type(args, c.var), inst_type(args, c.type), c.name)
			when FuncInstConstraint
				c_map = {}
				c.map.each_pair { |k, v| c_map[k] = inst_type(args, v) }
				FuncInstConstraint.new(c.ast, c_map)
			when PolyConstraint
				c.class.new(c.ast, inst_type(args, c.type), c.args.map { |arg| inst_type(args, arg) })
			else
				raise "Unknown constraint #{c.class}"
		end
	end
	
	def inst(func)
		map = {}
		inst_args = InstArgs.new(map, nil)
		infer_function func, @visited
		
		@constraints.concat func.constraints.map { |c| inst_constraint(inst_args, c) }
		
		vars = func.type_vars.map { |tv| map[tv] ||= new_var(tv.source) }
		
		@func_instances << [func, vars]
		
		return inst_type(inst_args, func.itype)
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
	
	def solve
		nil while @constraints.reject! do |c|
			case c
				when ApplyConstraint
					var = c.var.prune
					type = c.type.prune
					raise TypeError.new(recmsg(var, type)) if occurs_in?(var, type)
					case type
						when Types::Function
							unify(Types::Function.new(c.ast.source, c.args, var), type)
							
							true
						when Types::Variable
						else
							raise TypeError.new("'#{type.text}' is not a function type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
					end
				when FieldConstraint
					var = c.var.prune
					type = c.type.prune
					raise TypeError.new(recmsg(var, type)) if occurs_in?(var, type)
					case type
						when Types::Struct
							field = type.struct.scope.names[c.name]
							
							raise TypeError.new("'#{c.name.inspect}' is not a field in type '#{type.text}'\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}") unless field
							
							unify(inst_type(InstArgs.new({}, type), field.itype), var)
							
							true
						when Types::Variable
						else
							raise TypeError.new("'#{type.text}' is not a struct type\n#{c.ast.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
					end
				when PolyConstraint
					type = c.type.prune
					
					fixed = c.args.all? { |arg| arg.fixed_type? }
					
					if fixed
						true
					end
				when FuncInstConstraint
					var = c.var.prune
					if c.map.values.all? { |k| k.fixed_type? }
						unify(c.ast.create_instance(c.map).itype.result, var)
						true
					end
				else
					raise "Unknown constraint #{c.class}"
			end
		end
	end
	
	def self.create_function_instance(func, map)
		system = InferSystem.new
		system.create_function_instance(func, map)
	end
	
	def create_function_instance(func, map)
		full_map = map.dup
		@constraints = func.constraints.map { |c| inst_constraint(full_map, c) }
		type = inst_type(full_map, func.itype)
		solve
		raise "Unresolved contraints of #{func.name}" unless @constraints.empty?
		puts "map of #{func.name} #{full_map.each.map{|v| "#{v.first.text} => #{v.last.text}" }.join(", ")} with type #{type.text}"
		AST::Function::Instance.new(func, map, full_map, type)
	end
	
	def infer(func, visited)
		@visited = visited
		@func = func
	
		func.itype = @vars[func] = new_var(func.source)
		analyze(func)
		
		solve
		
		func.itype = func.itype.prune
		
		func.constraints = @constraints
		
		unless @constraints.empty?
			puts "constraints of #{func.name}"
			puts *@constraints.map{|c| " - #{c}"}
		end
		
		@constraints.each do |c|
			@type_vars.delete(c.var.prune)
		end
		
		@type_vars.reject! { |var| var.instance }
		
		func.type_vars = @type_vars.select { |var| occurs_in?(var, func.itype) }
		
		@type_vars -= func.type_vars
		
		unless @type_vars.empty?
			raise TypeError, "Unresolved vars of #{func.name}\n#{@type_vars.map{|c| " - #{c.text}"}.join("\n")}\n#{func.source.format}"
		end
		
		unless @func_instances.empty?
			puts "func instances of #{func.name}"
			puts *@func_instances.map{|i| " - #{i.first.name} (#{i.last.map(&:text).join(", ")})"}
		end
	end
end

def get_type(source, scope, ast)
	args = InferSystem::MakeTypeArgs.new(proc { |s| raise TypeError.new("Explicit type expected\n#{(s ? s : func.source).format}") }, proc { |ast, type, args|
		raise TypeError.new("Expected #{type.arg_count} type parameter(s) for #{type.text}, but got #{ast.args.size}\n#{ast.source.format}") if type.arg_count != ast.args.size
		type.template_dup(args)
	})
	InferSystem.make_type(source, scope, ast, args)
end

def infer_function(func, visited)
	return if func.itype
	raise "Recursive #{func.name}" if visited[func]
	visited[func] = true
	
	if func.scope
		InferSystem.new.infer(func, visited)
	else
		func.itype = get_type(func.source, func.parent_scope, func)
		func.constraints = []
		func.itype
	end
	
	puts "Type of #{func.name} is #{func.itype.text}"
end

def infer_scope(scope, params = {})
	raise "Recursive inference of scope" if scope.processed == :ongoing
	return if scope.processed
	scope.processed = :ongoing
	
	scope.names.each_pair do |name, value|
		case value
			when Types::Struct
				infer_scope(value.struct.scope, params)
			when AST::Variable
				value.itype = get_type(value.source, scope, value)
				puts "Type of #{value.name} is #{value.itype.text}"
			when AST::Function
				infer_function(value, params)
		end
	end
	scope.processed = true
end
