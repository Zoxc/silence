class InferSystem
	class TypeError < Exception
	end
	
	class Type
		attr_accessor :source
		
		def to_s
			"{#{text}}#{'s' if @source}"
			text
		end
		
		def text
			"<error>"
		end
		
		def args
			[]
		end
		
		def source_dup(source)
			result = dup
			result.source = source
			result
		end
	end
	
	class Fixed < Type
		attr_accessor :name
		
		def initialize(source, name)
			@source = source
			@name = name
		end
		
		def text
			@name
		end
	end
	
	class Variable < Type
		attr_accessor :instance
		
		def initialize(source, system, instance)
			@source = source
			@system = system
			@instance = instance
		end
		
		def text
			if @instance
				@instance.to_s
			else
				@name ||= @system.new_var_name
			end
		end
		
		def source
			if @instance
				@instance.source
			else
				@source
			end
		end
	end
	
	class Function < Type
		def initialize(source, args, result)
			@source = source
			@args = args
			@result = result
		end
		
		def name
			:func
		end
		
		def text
			"(#{@args.join(", ")}): #{@result}"
		end
		
		def args
			[*@args, @result]
		end
	end
	
	def initialize
		@int_type = Fixed.new(nil, 'int')
		@unit_type = Fixed.new(nil, 'unit')
		@bool_type = Fixed.new(nil, 'bool')
		@string_type = Fixed.new(nil, 'string')
		@results = {}
		@vars = {}
		@funcs = {}
		@var_name = 1
	end
	
	def new_var_name
		result = @var_name
		@var_name += 1
		"p#{result}"
	end
	
	def new_var(source = nil)
		Variable.new(source, self, nil)
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
				prev = @results[args.func]
				
				if prev
					unify(result, prev)
				else
					puts "settings result of #{args.func.name} to #{result.inspect}"
					@results[args.func] = result
				end
			when AST::If
				cond = analyze(ast.condition, args)
				unify(cond, @bool_type)
				
				analyze(ast.group, args)
				analyze(ast.else_node, args) if ast.else_node
				
				@unit_type
			when AST::BinOp
				lhs = analyze(ast.lhs, args)
				rhs = analyze(ast.rhs, args)
				unify(lhs, rhs)
				lhs
			when AST::Call
				type = @vars[ast.func]
				if type
					result = new_var(ast.source)
					unify(type, Function.new(ast.source, ast.args.map { |arg| analyze(arg, args) }, result))
					puts " result of call is #{result}"
					result
				else
					#todo fresh(ast.func)
				end
			when AST::Ref
				case ast.obj
					when AST::Variable
						@vars[ast.obj]
					else
						raise "Unknown Ref #{ast.obj.class}"
				end
			when AST::Literal
				case ast.type
					when :int
						@int_type
					when :bool
						@bool_type
					when :string
						@string_type
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source)
			when AST::Function
				@results[ast] = nil
				
				new_args = args.dup
				
				ast.scope.names.each_value do |var|
					@vars[var] = new_var
				end
				
				new_args.func = ast
				
				analyze(ast.scope, new_args)
				
				type = Function.new(ast.source, ast.params.map { |p| @vars[p.var] }, @results[ast] || @unit_type).source_dup(ast.source)
				unify(type, @vars[ast])
				type
			when AST::Scope
				if ast.nodes.empty?
					@unit_type
				else
					ast.nodes[0...-1].each do |node|
						analyze(node, args)
					end
					analyze(ast.nodes.last, args)
				end
			else
				raise "Unknown AST #{ast.class}"
		end
	end
	
	def prune(type)
		if type.is_a?(Variable) && type.instance
			pruned = prune(type.instance)
			type.source = type.source || pruned.source
			type.instance = pruned
		else
			type
		end
	end
	
	def occurs_in?(a, b)
		return true if a == b
		return false if b.is_a? Variable
		
		return b.args.any? do |arg|
			occurs_in?(a, prune(arg))
		end
	end
	
	def errmsg(a, b)
		return errmsg(b, a) if b.source && !a.source
		
		if a.source && b.source
			"Expression of type #{a},\n#{a.source.format}\nconflicts with expression of type #{b},\n#{b.source.format}" 
		elsif a.source
			"Expected type #{b}, but found type #{a}\n#{a.source.format}" 
		else
			"Expression of type #{a}, conflicts with expression of type #{b}" 
		end
	end
	
	def recmsg(a, b)
		source = a.source || b.source
		
		if a.source && b.source
			"Recursive type #{a},\n#{a.source.format}\ntype #{b},\n#{b.source.format}" 
		elsif source
			"Recursive type #{a}, occurs in type #{b}\n#{source.format}" 
		else
			"Recursive type #{a}, occurs in type #{b}" 
		end
	end
	
	def unify(a, b)
		a = prune(a)
		b = prune(b)
		
		puts "unifying #{a} and #{b}"
		
		if a.is_a? Variable
			raise TypeError.new(recmsg(a, b)) if occurs_in?(a, b)
			a.instance = b
			a.source = a.source || b.source
			return
		end
		
		return unify(b, a) if b.is_a? Variable
		
		a_args = a.args
		b_args = b.args
		
		raise TypeError.new(errmsg(a, b)) if (a.name != b.name) || (a_args.size != b_args.size)
		
		a_args.each_with_index do |a_arg, i|
			unify(a_arg, b_args[i])
		end
	end
	
	def infer(funcs)
	
		puts "Running type inference on the set: #{funcs.map(&:name).inspect}"
		
		funcs.each { |func| @vars[func] = new_var.source_dup(func.source) }
		funcs.each { |func| analyze(func) }
		
		funcs.each do |func|
			puts "Type of #{func.name} is #{@vars[func]}"
		end
		
		puts "Done running type inference."
	end
end

def infer_functions(funcs)
	puts "Running type inference on: #{funcs.map(&:name).inspect}"
	
	puts "Done running type inference."
end

class InferParams
	attr_accessor :visited, :lists, :stack
	
	def initialize
		@visited = {}
		@lists = {}
		@stack = []
	end
end

def infer_function(func, params)
	if (i = params.stack.index func) != nil
		list = params.stack[i..-1].uniq
		list.each do |entry|
			params.lists[entry] = list
		end
		return
	end
	
	return if params.visited[func]
	params.visited[func] = true
	return unless func.scope

	params.stack << func
	
	func.scope.uses.each do |value|
		case value
			when AST::Function
				infer_function value, params
		end
	end
	
	params.stack.pop
	
	funcs = params.lists[func] || [func]
	return if funcs.first != func
	
	InferSystem.new.infer(funcs)
end

def infer_scope(ast, params = InferParams.new)
	ast.scope.names.each_pair do |name, value|
		case value
			when AST::Function
				infer_function value, params
		end
	end
end
