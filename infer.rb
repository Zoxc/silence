﻿class InternalType
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
	
	class InterfaceLimit < Limit
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
			when AST::Function::Parameter
				make_type(ast.source, scope, ast.type)
			when AST::NullPtrTypeNode, AST::PtrTypeNode
				make_type(ast.source, scope, ast.target)
			when AST::NamedTypeNode
				type = scope.require(ast.source, ast.name, Types::Type)
				
				if type.template?
					args = ast.args.map { |n| make_type(ast.source, scope, n) }
					if type.interface?
						interface_result = new_var(ast.source)
						args.unshift(interface_result) 
					end
					raise TypeError.new("Too many type parameter(s) for #{type.text}, got #{type.arg_count} but maximum is #{args.size}\n#{ast.source.format}") if type.arg_count < args.size
					template_args = type.arg_count.times.map { |i| args[i] || new_var(ast.source) }
					result = type.template_dup(template_args)
					if type.interface?
						@limits << InterfaceLimit.new(self, ast, result)
						@views[interface_result] = result
						interface_result
					else
						result
					end
				else
					raise TypeError.new("Unexpected type parameter(s) for non-template type #{type.text}\n#{ast.source.format}") unless type.args.empty?
					
					if type.kind_of? Types::TemplateParam
						@template_vars[type.param] ||= new_var(ast.source)
					else
						type
					end
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
				interface = Types::FunctionClass.template_dup([type, Types::TypePack.new(ast.source, call_args)]).source_dup(ast.source)
				limit = InterfaceLimit.new(self, ast, interface)
				@limits << limit
				@limits << TypeFunctionLimit.new(self, ast, result, AST::FunctionClassResult, limit)
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
						Types::IntType
					when :bool
						Types::BoolType
					when :string
						Types::StringType
					else
						raise "Unknown literal type #{ast.type}"
				end.source_dup(ast.source)
			when AST::Function
				@result = (make_type(ast.source, @obj.scope, ast.result) if ast.result)
				
				new_args = args.dup
				
				ast.scope.names.each_value do |var|
					@vars[var] = make_type(ast.source, @obj.scope, var.type)
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
	
	InstArgs = Struct.new(:map, :lmap, :base)
	
	def inst_limit(args, limit)
		case limit
			when InterfaceLimit
				args.lmap[limit] ||= InterfaceLimit.new(self, limit.ast, inst_type(args, limit.var))
			when TypeFunctionLimit
				TypeFunctionLimit.new(self, limit.ast, inst_type(args, limit.var), limit.type_ast, inst_limit(args, limit.interface))
			else
				raise "Unknown limit #{limit.class}"
		end
	end
	
	def inst_type(args, type)
		type = type.prune
		case type
			when Types::Fixed
				type
			when Types::Variable
				args.map[type] ||= new_var(type.source)
			when Types::Function
				type.args_dup(*type.args.map { |arg| inst_type(args, arg) })
			when Types::Struct, Types::TypePack
				type.template_dup(type.args.map { |arg| inst_type(args, arg) })
			when Types::TypeFunction
				type.function_dup(inst_type(args, type.interface))
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
				index = base.struct.template_params.index(param)
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
				when InterfaceLimit
				when TypeFunctionLimit
				when FieldLimit
					var = c.var.prune
					type = c.type.prune
					type = view(type) if type.is_a? Types::Variable
					raise TypeError.new(recmsg(var, type)) if occurs_in?(var, type)
					case type
						when Types::Struct
							field = type.struct.scope.names[c.name]
							
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
		
		#raise "Unresolved contraints of #{@obj.name}" unless @limits.empty?
		
		@infer_args = nil
		self
	end
	
	def infer_fixed(obj, scope)
		@obj = obj
		@type = make_type(obj.source, scope, obj)
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

def get_type(source, scope, ast)
	InternalType.new.infer_fixed(ast, scope)
end

InferArgs = Struct.new :visited

module InferUtils
	def self.infer_function(func, args)
		return if func.itype
		raise "Recursive #{func.name}" if args.visited[func]
		args.visited[func] = true
		
		if func.scope
			InternalType.new.infer_function(func, args)
		else
			func.itype = get_type(func.source, func.parent_scope, func)
		end
		
		puts "Type of #{func.name} is #{func.itype.type.text}"
	end

	def self.infer_var(var, scope, args)
		return if var.itype
		raise "Recursive #{var.name}" if args.visited[var]
		args.visited[var] = true
		var.itype = get_type(var.source, scope, var)
		puts "Type of #{var.name} is #{var.itype.type.text}"
	end

	def self.infer_type(value, scope, args)
		case value
			when Types::Struct
				infer_scope(value.struct.scope, args)
			when AST::Variable
				infer_var(value, scope, args)
			when AST::Function
				infer_function(value, args)
			else
				raise "Unknown value #{value.class}"
		end
	end

	def self.infer_scope(scope, args = InferArgs.new({}))
		scope.names.each_pair do |name, value|
			next if value.kind_of? Types::TemplateParam
			infer_type(value, scope, args)
		end
	end
end
