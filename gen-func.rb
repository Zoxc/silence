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
		raise "Expected type" unless type.is_a?(Types::Type)
		
		if var
			var.type = type
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
			assign_var(var, ast, nil)
			assign_func(var, nil, ref)
		else
			assign_var(var, ast, ref)
		end
	end
	
	def lvalue(ast)
		case ast
			when AST::UnaryOp
				ptr = new_var
				convert(ast.node, ptr)
				return [:single, "*#{ptr.ref}", ptr]
			when AST::Field
				case ast.gen[:type]
					when :single
						return ref(ast.gen[:ref], ast.gen[:args].last.params)
					when :field
						single, lval, lvar = lvalue(ast.obj)
						return [:single, "(#{lval}).f_#{ast.gen[:ref].name}", lvar]
				end
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Function)
					return [:single, "v_#{ast.obj.name}"]
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					return [:single, "self->f_#{ast.obj.name}"]
				else
					return [:single, ref(ast.obj, ast.gen.last.params.dup.merge(@map.params))]
				end
			when AST::Tuple
				[:tuple, ast.nodes.map { |n| lvalue(n) }]
			else
				raise "Unknown"
		end
	end
	
	def direct_call(var, ref, obj, args, result_type)
		rvar = var ? var : new_var
		assign_var(rvar, result_type, nil)
		o "#{ref}(#{obj ? "&#{obj.ref}" : "0"}, &#{rvar.ref}#{args.map{|a| ", #{a}"}.join});"
		del_var rvar unless var
	end
	
	def assign(var, type, lvalue_result, rhs)
		assign_var(var, type, nil) if var
		kind, lval, tvar = lvalue_result
		case kind
			when :single
				destroy_var(lval, type)
				copy_var(rhs, lval, type)
				copy_var(lval, var.ref, type) if var
				del_var tvar
			when :tuple
				tuple_map = type.tuple_map
				lval.each_with_index do |v, i|
					assign(nil, tuple_map[i], v, "(#{rhs}).f_#{i}")
				end
				copy_var(rhs, var.ref, type) if var
		end
	end
	
	def convert(ast, var)
		case ast
			when AST::Scope
				nodes = ast.nodes.compact
				
				if nodes.empty?
					assign_var(var, Core::Unit.ctype.type, nil)
				else
					nodes[0...-1].each do |e|
						convert(e, nil)
					end
					convert(nodes.last, var)
				end
			when AST::VariableDecl
				convert(ast.value, nil) if ast.value
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::Literal
				assign_var(var, ast.gtype, case ast.type
					when :int
						direct_call(var, ref(Core::IntLiteral::Create, {Core::IntLiteral::T => ast.gtype}), nil, [ast.value.to_s], ast.gtype)
						nil
					when :bool
						ast.value.to_s
					when :string
						direct_call(var, ref(Core::StringLiteral::Create, {Core::StringLiteral::T => ast.gtype}), nil, ["(_char *)#{ast.value.inspect}", "#{ast.value.size}"], ast.gtype)
						nil
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
						single, lval, tvar = lvalue(ast.node)
						assign_var(var, ast.gtype, "&(#{lval})")
						del_var tvar
				end
			when AST::Field
				case ast.gen[:type]
					when :single
						assign_f(var, ast.gtype, ast.gen[:ref], ast.gen[:args].params)
					when :field
						single, lval, lvar = lvalue(ast.obj)
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
					assign_f(var, ast.gtype, ast.obj, ast.gen.last.params.dup.merge(@map.params))
				end
			when AST::Return # TODO: Ensure all variables in scope get's destroyed on return
				result = new_var
				convert(ast.value, result)
				o "*result = #{result.ref};"
				o "return;"
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::BinOp
				rhs = new_var
				convert(ast.rhs, rhs)
				if ast.op == '='
					assign(var, ast.gtype, lvalue(ast.lhs), rhs.ref)
				else
					lhs = new_var
					convert(ast.lhs, lhs)
					
					typeclass = Core::OpMap[ast.op]
					
					if typeclass
						lhs_arg = new_var
						rhs_arg = new_var
						copy_var(lhs.ref, lhs_arg.ref, ast.gtype)
						copy_var(rhs.ref, rhs_arg.ref, ast.gtype)
						assign_var(lhs_arg, ast.gtype, nil)
						assign_var(rhs_arg, ast.gtype, nil)
						direct_call(var, ref(typeclass[:func], {typeclass[:param] => ast.gtype}), nil, [lhs_arg.ref, rhs_arg.ref], ast.gtype)
						del_var lhs_arg
						del_var rhs_arg
					else
						assign_var(var, ast.gtype, lhs.ref + " #{ast.op} " + rhs.ref)
					end
					
					del_var lhs
				end
				del_var rhs
			when AST::If
				result = "if(#{gen_body(ast.condition)})\n#{ind.()}{\n" + gen_body.(ast.group) + "\n#{ind.()}}"
				result << "\nelse\n#{ind.()}{\n" + gen_body.(ast.else_node) + "\n#{ind.()}}" if ast.else_node
			when AST::Grouped
				convert(ast.node, var)
			when AST::Call
				if ast.gen.first
					obj = new_var
					convert(ast.obj, obj) 
				end
						
				args = new_var
				arg_vars = []
				arg_types = []
				ast.args.each_with_index do |a, i|
					arg = new_var
					arg_type = a.gtype
					arg_types << arg_type
					arg_vars << arg
					convert(a, arg)
					copy_var(arg.ref, "#{args.ref}.f_#{i}", arg_type)
				end
				assign_var(args, ast.gen.last, nil)
				
				if ast.gen.first
					direct_call(var, ref(Core::Callable::Apply, {Core::Callable::T => ast.obj.gtype}), obj, [args.ref], ast.gtype)
				else
					rvar = var ? var : new_var
					assign_var(rvar, ast.gtype, nil)
					direct_call(nil, ref(Core::Constructor::Construct, {Core::Constructor::T => ast.obj.gtype}), nil, ["&#{rvar.ref}", args.ref], Core::Unit.ctype.type)
					del_var rvar unless var
				end
				
				arg_vars.each { |v| del_var v }
				del_var args
				del_var obj if ast.gen.first
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end
end
