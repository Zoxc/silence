class FuncCodegen
	attr_reader :gen, :func
	
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
		
		def decl
			"    #{type ? type : "ERROR"} #{ref};\n"
		end
	end
	
	def new_var
		var = Var.new(@var_name += 1, nil)
		@vars << var
		var
	end
	
	def assign_var(var, type, str)	
		var.type = gen.c_type(type.first, @map)
		(o var.ref + " = " + str + ";") if str
	end
	
	def process
		convert(func.scope, new_var)
		@vars.map(&:decl).join + "\n" + @out.join("\n")
	end
	
	def resolve_limit(limit)
		return limit if limit.is_a?(TypeContext::InstanceStruct)
		limit = @gen.fixup_limit(@func, limit)
		return limit if limit.is_a?(TypeContext::InstanceStruct)
		r = @map.limits[limit]
		raise "Unable to find instance for limit #{limit}" unless r
		r
	end
	
	def resolve_limit_inst(limit)
		v = resolve_limit(limit)
		puts "resolve_limit_inst #{limit} as #{v}"
		inst = @gen.inst_type(v.inst, @map)
		TypeContext::InstanceStruct.new(inst, resolve_limits(v.map))
	end
	
	def resolve_limits(limits)
		r = {}
		
		limits.each do |k, v|
			r[k] = resolve_limit_inst(v)
		end
		
		r
	end
	
	def ref(obj, context_map, tc_limit = nil)
		map = TypeContext::Map.new({}, {}, {})
		
		context_map.params.each do |k, v|
			map.params[k] = @gen.inst_type(v, @map)
		end
		
		map.limits = resolve_limits(context_map.limits)
		
		@gen.ref(obj, map, tc_limit ? resolve_limit_inst(tc_limit) : nil)
	end
	
	def o(str)
		@out << ("    " + str)
	end
	
	def assign_func(var, data, ref)
		o var.ref + ".func = " + ref + ";"
		(o var.ref + ".data = &" + data + ";") if data
	end
	
	def assign_f(var, ast, obj, context_map)
		ref = ref(obj, context_map)
		
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
				return "*#{ptr.ref}"
			when AST::Field
				case ast.gen[:type]
					when :single
						return ref(ast.gen[:ref], ast.gen[:args].last)
					when :field
						obj = new_var
						convert(ast.obj, obj)
						return "#{obj.ref}.f_#{ast.gen[:ref].name}"
				end
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Function)
					return "v_#{ast.obj.name}"
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					return "self->f_#{ast.obj.name}"
				else
					return ref(ast.obj, TypeContext::Map.new({}, ast.gen.last.params.dup.merge(@map.params), ast.gen.last.limits.dup.merge(@map.limits)))
				end
		end
	end
	
	def convert(ast, var)
		case ast
			when AST::Scope
				if ast.nodes.empty?
					assign_var(var, [Core::Unit.ctype.type, true], nil)
				else
					ast.nodes[0...-1].each do |e|
						convert(e, new_var)
					end
					convert(ast.nodes.last, var)
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
						assign_var(var, ast.gtype, lvalue(ast))
					when '&'
						assign_var(var, ast.gtype, '&' + lvalue(ast.node))
				end
			when AST::Field
				case ast.gen[:type]
					when :single
						assign_f(var, ast, ast.gen[:ref], ast.gen[:args])
					when :field
						obj = new_var
						convert(ast.obj, obj)
						
						if ast.gen[:ref].is_a?(AST::Function)
							assign_var(var, ast.gtype, nil)
							assign_func(var, obj.ref, ref(ast.gen[:ref], ast.gen[:args], ast.gen[:tc_limit]))
						else
							assign_var(var, ast.gtype, "#{obj.ref}.f_#{ast.gen[:ref].name}")
						end
				end
			when AST::Index
				convert(ast.obj, var)
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Function)
					assign_var(var, ast.gtype, "v_#{ast.obj.name}")
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					assign_var(var, ast.gtype, "self->f_#{ast.obj.name}") # TODO: Check for the case when accesing a field in a parent struct
				else
					assign_f(var, ast, ast.obj, TypeContext::Map.new({}, ast.gen.last.params.dup.merge(@map.params), ast.gen.last.limits.dup.merge(@map.limits)))
				end
			when AST::Return
				result = new_var
				convert(ast.value, result)
				o "*result = #{result.ref};"
				o "return;"
				assign_var(var, [Core::Unit.ctype.type, true], nil)
			when AST::BinOp
				rhs = new_var
				convert(ast.rhs, rhs)
				if ast.op == '='
					assign_var(var, ast.gtype, lvalue(ast.lhs) + " = " + rhs.ref)
				else
					lhs = new_var
					convert(ast.lhs, lhs)
					assign_var(var, ast.gtype, lhs.ref + " #{ast.op} " + rhs.ref)
				end
			when AST::If
				result = "if(#{gen_body(ast.condition)})\n#{ind.()}{\n" + gen_body.(ast.group) + "\n#{ind.()}}"
				result << "\nelse\n#{ind.()}{\n" + gen_body.(ast.else_node) + "\n#{ind.()}}" if ast.else_node
			when AST::Grouped
				convert(ast.node, var)
			when AST::Call
				obj = new_var
				convert(ast.obj, obj)
				
				args = new_var
				ast.args.each_with_index do |a, i|
					arg = new_var
					convert(a, arg)
					o "#{args.ref}.f_#{i} = #{arg.ref};"
				end
				assign_var(var, ast.gtype, nil)
				assign_var(args, [ast.gen[:args]], nil)
				
				# TODO: Find out which typeclass instances to pass along here
				map = TypeContext::Map.new({}, {Core::Callable::T => ast.obj.gtype.first}, {})
				o "#{ref(Core::Callable::Apply, map, ast.gen[:tc_limit])}(&#{obj.ref}, &#{var.ref}, #{args.ref});"
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end
end
