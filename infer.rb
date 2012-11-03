class InferSystem
	class TypeError < Exception
	end
	
	class Type
		attr_accessor :source
		
		def to_s
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
		
		def to_s
			@name
		end
	end
	
	class Variable < Type
		attr_accessor :instance
		
		def initialize(system, instance)
			@system = system
			@instance = instance
		end
		
		def to_s
			if @instance
				@instance.to_s
			else
				@name ||= @system.new_var_name
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
		
		def to_s
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
		@var_name = 'α'
	end
	
	def new_var_name
		result = @var_name
		@var_name = (@var_name.ord + 1).chr(Encoding::UTF_8)
		result
	end
	
	def new_var
		Variable.new(self, nil)
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
				# todo
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
				
				Function.new(ast.source, ast.params.map { |p| @vars[p.var] }, @results[ast] || @unit_type)
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
			type.instance = prune(type.instance)
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
	
	def unify(a, b)
		a = prune(a)
		b = prune(b)
		
		puts "unifying #{a} and #{b}"
		
		return unify(b, a) if b.is_a? Variable
		
		if a.is_a? Variable
			raise "Recursive unification" if occurs_in?(a, b)
			a.instance = b
			return
		end
		
		a_args = a.args
		b_args = b.args
		
		raise TypeError.new(errmsg(a, b)) if (a.name != b.name) || (a_args.size != b_args.size)
		
		a_args.each_with_index do |a_arg, i|
			unify(a_arg, b_args[i])
		end
	end
	
	def infer(funcs)
	
		puts "Running type inference on the set: #{funcs.map(&:name).inspect}"
		
		results = funcs.map do |func|
			[func, analyze(func)]
		end
		
		results.each do |result|
			puts "Type of #{result.first.name} is #{result.last}"
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
