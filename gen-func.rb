class FuncCodegen
	attr_reader :gen, :func, :map
	
	def initialize(gen, func, map)
		@gen = gen
		@func = func
		@out = []
		@map = map
		@var_name = 0
		@vars = []
	end
	
	Var = Struct.new(:name, :type) do
		def ref
			"t_#{name}"
		end
		
		def decl(fgen)
			"    #{type ? fgen.gen.c_type(type, fgen.map) : "ERROR"} #{ref};\n"
		end
	end
	
	def new_var
		var = Var.new(@var_name += 1, nil)
		@vars << var
		var
	end

	def copy_var(src, dst, type)
		o "#{dst} = #{src}; // copy(#{gen.c_type(type, @map)})"
	end
	
	def destroy_var(var, type)
		o "// destroy #{var} :: #{gen.c_type(type, @map)}"
	end
	
	def del_var(var)
		return unless var
		destroy_var(var.ref, var.type)
	end
	
	def assign_var(var, type, str)
		if var
			var.type = type.first
			(o var.ref + " = " + str + ";") if str
		else
			(o str + ";") if str
		end
	end
	
	def process
		var = new_var
		convert(func.scope, var)
		del_var var
		
		param_types = @func.ctype.type.args[Core::Func::Args].tuple_map.reverse
		@func.params.reverse.each_with_index { |p, i| destroy_var("v_#{p.name}", param_types[i]) }
		
		@vars.map { |v| v.decl(self) }.join + "\n" + @out.join("\n")
	end
	
	def ref(obj, params)
		map = {}
		
		params.each do |k, v|
			map[k] = @gen.inst_type(v, @map)
		end
		
		owner = obj.declared.owner
		
		if owner.is_a?(AST::TypeClass)	
			ref, new_map = @gen.find_instance(owner.ctype.type, TypeContext::Map.new({}, map), obj)
			
			ref.type_params.each_with_index do |p, i|
				param = obj.type_params[i]
				raise "Didn't find matching param for '#{p.name}'" unless param
				param_mapped = map.params[param]
				raise "Param '#{p.name}' type not provided" unless param_mapped
				new_map.params[p] = param_mapped
			end
			
			obj = ref
			map = new_map
		end
		
		@gen.ref(obj, map)
	end
	
	def o(str)
		@out << ("    " + str)
	end
	
	def assign_func(var, data, ref)
		o var.ref + ".func = " + ref + ";"
		(o var.ref + ".data = &" + data + ";") if data
	end
	
	def assign_f(var, ast, obj, params)
		ref = ref(obj, params)
		
		if obj.is_a?(AST::Function)
			assign_var(var, ast.gtype, nil)
			assign_func(var, nil, ref)
		else
			assign_var(var, ast.gtype, ref)
		end
	end
	
	def lvalue(ast)
		case ast
			when AST::UnaryOp
				ptr = new_var
				convert(ast.node, ptr)
				return ["*#{ptr.ref}", ptr]
			when AST::Field
				case ast.gen[:type]
					when :single
						return ref(ast.gen[:ref], ast.gen[:args].last.params)
					when :field
						lval, lvar = lvalue(ast.obj)
						return ["(#{lval}).f_#{ast.gen[:ref].name}", lvar]
				end
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Function)
					return ["v_#{ast.obj.name}"]
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					return ["self->f_#{ast.obj.name}"]
				else
					return [ref(ast.obj, ast.gen.last.params.dup.merge(@map.params))]
				end
		end
	end
	
	def call(var, obj, obj_type, args, result_type)
		args = new_var
		arg_vars = []
		arg_types = []
		args.each_with_index do |a, i|
			arg = new_var
			arg_vars << arg
			convert(a, arg)
			arg_type = a.gtype.first
			arg_types << arg_type
			copy_var(arg.ref, "#{args.ref}.f_#{i}", arg_type)
		end
		assign_var(var, [result_type], nil)
		assign_var(args, [InferContext.make_tuple(Core.src, arg_types)], nil)
		
		o "#{ref(Core::Callable::Apply, {Core::Callable::T => obj_type})}(&#{obj.ref}, #{var ? "&#{var.ref}" : "0"}, #{args.ref});"
		arg_vars.each { |v| del_var v }
		del_var args
	end
	
	def convert(ast, var)
		case ast
			when AST::Scope
				nodes = ast.nodes.compact
				
				if nodes.empty?
					assign_var(var, [Core::Unit.ctype.type], nil)
				else
					nodes[0...-1].each do |e|
						convert(e, nil)
					end
					convert(nodes.last, var)
				end
			when AST::Literal
				assign_var(var, ast.gtype, case ast.type
					when :int, :bool
						ast.value.to_s
					when :string
						"(_char *)#{ast.value.inspect}"
					else
						raise "Unknown literal type #{ast.type}"
				end)
			when AST::UnaryOp
				case ast.op
					when '*'
						ptr = new_var
						convert(ast.node, ptr)
						assign_var(var, ast.gtype, "*#{ptr.ref}")
						del_var ptr
					when '&'
						lval, tvar = lvalue(ast.node)
						assign_var(var, ast.gtype, "&(#{lval})")
						del_var tvar
				end
			when AST::Field
				case ast.gen[:type]
					when :single
						assign_f(var, ast, ast.gen[:ref], ast.gen[:args].params)
					when :field
						lval, lvar = lvalue(ast.obj)
						if ast.gen[:ref].is_a?(AST::Function)
							assign_var(var, ast.gtype, nil)
							assign_func(var, lval, ref(ast.gen[:ref], ast.gen[:args].params))
						else
							assign_var(var, ast.gtype, "(#{lval}).f_#{ast.gen[:ref].name}")
						end
						del_var lvar
				end
			when AST::Index
				convert(ast.obj, var)
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Function)
					assign_var(var, ast.gtype, "v_#{ast.obj.name}")
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					assign_var(var, ast.gtype, "self->f_#{ast.obj.name}") # TODO: Check for the case when accesing a field in a parent struct
				else
					assign_f(var, ast, ast.obj, ast.gen.last.params.dup.merge(@map.params))
				end
			when AST::Return # TODO: Ensure all variables in scope get's destroyed on return
				result = new_var
				convert(ast.value, result)
				o "*result = #{result.ref};"
				o "return;"
				assign_var(var, [Core::Unit.ctype.type, true], nil)
			when AST::BinOp
				rhs = new_var
				convert(ast.rhs, rhs)
				if ast.op == '='
					lval, tvar = lvalue(ast.lhs)
					destroy_var(lval, ast.gtype.first)
					copy_var(rhs.ref, lval, ast.gtype.first)
					assign_var(var, ast.gtype, lval)
					del_var tvar
				else
					lhs = new_var
					convert(ast.lhs, lhs)
					assign_var(var, ast.gtype, lhs.ref + " #{ast.op} " + rhs.ref)
					del_var lhs
				end
				del_var rhs
			when AST::If
				result = "if(#{gen_body(ast.condition)})\n#{ind.()}{\n" + gen_body.(ast.group) + "\n#{ind.()}}"
				result << "\nelse\n#{ind.()}{\n" + gen_body.(ast.else_node) + "\n#{ind.()}}" if ast.else_node
			when AST::Grouped
				convert(ast.node, var)
			when AST::Call
				obj = new_var
				convert(ast.obj, obj)
				#call(var, obj, ast.obj.gtype.first, ast.args, ast.gtype.first)
				del_var obj
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end
end
