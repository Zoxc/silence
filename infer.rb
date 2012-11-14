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
			"Apply{ #{@var.text} == #{@type.text} (#{@args.map(&:text).join(", ")}) }"
		end
	end
	
	def initialize
		@vars = {}
		@type_vars = []
		@var_name = 1
		@constraints = []
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
	
	class AnalyzeArgs
		attr_accessor :func

		def initialize
			@func = nil
		end
	end
	
	def self.make_type(source, scope, ast, new_var)
		return new_var.(source) unless ast
		case ast
			when AST::Function
				Types::Function.new(ast.source, ast.params.map { |p| make_type(ast.source, scope, p, new_var) }, make_type(ast.source, scope, ast.result, new_var))
			when AST::Function::Parameter
				make_type(ast.source, scope, ast.type, new_var)
			when AST::NullPtrTypeNode, AST::PtrTypeNode
				make_type(ast.source, scope, ast.target, new_var)
			when AST::NamedTypeNode
				scope.require(ast.source, ast.name, Types::Type)
		end
	end
	
	def make_type(source, args, ast)
		InferSystem.make_type(source, @func.scope, ast, proc { |s| new_var(s) })
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
					@vars[var] = make_type(ast.source, args, var.type) || new_var
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
	
	def prune(type)
		if type.is_a?(Types::Variable) && type.instance
			pruned = prune(type.instance)
			type.source = type.source || pruned.source
			type.instance = pruned
		else
			type
		end
	end
	
	def occurs_in?(a, b)
		return true if a == b
		return false if b.is_a? Types::Variable
		
		return b.args.any? do |arg|
			occurs_in?(a, prune(arg))
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
	
	def inst(func)
		map = {}
		infer_function func, @visited
		
		inst_type = proc do |type|
			type = prune(type)
			case type
				when Types::Fixed
					type
				when Types::Variable
					map[type] ||= new_var(type.source)
				when Types::Function
					type.args_dup(*type.args.map { |arg| inst_type.(arg) })
				else
					raise "Unknown type #{type.class}"
			end
		end
		
		inst_constraint = proc do |c|
			case c
				when ApplyConstraint
					ApplyConstraint.new(c.ast, inst_type.(c.var), inst_type.(c.type), c.args.map { |arg| inst_type.(arg) })
				else
					raise "Unknown constraint #{c.class}"
			end
		end
		
		@constraints.concat(func.constraints.map { |c| inst_constraint.(c) })
		
		return inst_type.(func.itype)
	end
	
	def unify(a, b, loc = proc { "" })
		a = prune(a)
		b = prune(b)
		
		if a.is_a? Types::Variable
			raise TypeError.new(recmsg(a, b) + loc.()) if occurs_in?(a, b)
			a.instance = b
			a.source = a.source || b.source
			return
		end
		
		return unify(b, a, loc) if b.is_a? Types::Variable
		
		a_args = a.args
		b_args = b.args
		
		raise TypeError.new(errmsg(a, b) + loc.()) if (a.name != b.name) || (a_args.size != b_args.size)
		
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
					var = prune(c.var)
					type = prune(c.type)
					raise TypeError.new(recmsg(var, type)) if occurs_in?(var, type)
					case type
						when Types::Function
							unify(Types::Function.new(c.ast.source, c.args, var), type)
							
							true
						when Types::Variable
						else
							raise TypeError.new("#{type.text} is not a function type\n#{c.ast.obj.source.format}#{"\nType inferred from:\n#{type.source.format}" if type.source}")
					end
			end
		end
	end
	
	def infer(func, visited)
		@visited = visited
		@func = func
	
		func.itype = @vars[func] = new_var(func.source)
		analyze(func)
		
		solve
		
		func.constraints = @constraints
		
		unless @constraints.empty?
			puts "constraints of #{func.name}"
			puts *@constraints.map{|c| " - #{c}"}
		end
		
		@constraints.each do |c|
			@type_vars.delete(c.var)
		end
		
		@type_vars.reject! do |var|
			var.instance || occurs_in?(var, prune(func.itype))
		end
		
		unless @type_vars.empty?
			puts "unresolved vars of #{func.name}"
			puts *@type_vars.map{|c| " - #{c.text}"}
		end
	end
end

def infer_function(func, visited)
	return if func.itype
	raise "Recursive #{func.name}" if visited[func]
	visited[func] = true
	
	if func.scope
		InferSystem.new.infer(func, visited)
	else
		func.itype = InferSystem.make_type(func.source, func.parent_scope, func, proc { |s| raise TypeError.new("Explicit type expected for prototype\n#{(s ? s : func.source).format}") })
		func.constraints = []
		func.itype
	end
	
	puts "Type of #{func.name} is #{func.itype.text}"
end

def infer_scope(ast, params = {})
	ast.scope.names.each_pair do |name, value|
		case value
			when AST::Function
				infer_function value, params
		end
	end
end
