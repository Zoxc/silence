class FuncCodegen
	attr_reader :gen, :func, :map
	
	def initialize(gen, func, map)
		@gen = gen
		@func = func
		@out = []
		@map = map
		@var_name = 0
		@vars = []
		@current_vars = []
		@decl_map = {}
	end
	
	Var = Struct.new(:name, :type) do
		def ref
			"t_#{name}"
		end
		
		def decl(fgen)
			"    #{type ? fgen.gen.c_type(type, fgen.map) : "ERROR"} #{ref};\n"
		end
	end
	
	RealVar = Struct.new(:name, :type) do
		def ref
			"#{name}"
		end
	end
	
	def new_var(&block)
		var = Var.new(@var_name += 1, nil)
		@vars << var
		@current_vars.push(var)
		var
	end

	def copy_var(src, dst, type)
		ref = gen.ref_action(type, @map, :copy)
		o "#{dst} = #{src}; #{"#{ref}(&(#{dst}));" if ref} // copy"
	end
	
	def destroy_var(var, type)
		ref = gen.ref_action(type, @map, :destroy)
		o "#{"#{ref}(&(#{var}));" if ref} // destroy #{var}"
	end
	
	def pop_var(name)
		pop = @current_vars.pop
		raise "Mismatch (got #{pop.ref}, expected #{name.ref})" unless pop.eql?(name)
	end
	
	def del_var(var, destroy = true)
		return unless var
		pop_var(var)
		destroy_var(var.ref, var.type) if destroy
	end
	
	def assign_var(var, type, str, copy = false)
		raise "Expected type, got #{type.class}" unless type.is_a?(Types::Type)
		
		if var
			var.type = type
			(o var.ref + " = " + str + ";") if str
		else
			(o str + ";") if str
		end
		
		copy_var(var.ref, var.ref, type) if var && copy
	end
	
	def process
		params = @func.ctype.type.args[Core::Func::Args].tuple_map.zip(@func.params)
		params = params.map { |type, p| var = RealVar.new("v_#{p.name}", type); @current_vars.push(var); var }
		convert(func.scope, nil)
		params.reverse.each do |var|
			pop_var(var)
			destroy_var(var.ref, var.type)
		end
		raise "Undestroyed variables:\n#{@current_vars.map(&:ref).join("\n")}" unless @current_vars.empty?
		
		if @func.is_a?(AST::Function)
			param_types = @func.ctype.type.args[Core::Func::Args].tuple_map.reverse
			@func.params.reverse.each_with_index { |p, i| destroy_var("v_#{p.name}", param_types[i]) }
		end
		
		@vars.map { |v| v.decl(self) }.join + "\n" + @out.join("\n")
	end
	
	def ref(obj, params)
		map = {}
		
		params.each do |k, v|
			map[k] = @gen.inst_type(v, @map)
		end if params
		
		owner = obj.declared.owner
		
		if owner.is_a?(AST::TypeClass)	
			ref, new_map = @gen.find_instance(owner.ctype.type, TypeContext::Map.new({}, map), obj)
			
			ref.kind.params.each_with_index do |p, i|
				param = obj.kind.params[i]
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
			assign_func(var, nil, ref)
			assign_var(var, ast, nil, true)
		else
			assign_var(var, ast, ref, true)
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
						return [:single, ref(ast.gen[:ref], ast.gen[:args].last.params)]
					when :field
						single, lval, lvar = lvalue(ast.obj)
						return [:single, "(#{lval}).f_#{ast.gen[:ref].name}", lvar]
				end
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner == @func
					return [:single, "v_#{ast.obj.name}"]
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					return [:single, "self->f_#{ast.obj.name}"]
				else
					return [:single, ref(ast.obj, ast.gen.last.params)]
				end
			when AST::Tuple
				[:tuple, ast.nodes.reverse.map { |n| lvalue(n) }]
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
				lval.reverse.each_with_index do |v, i|
					assign(nil, tuple_map[i], v, "(#{rhs}).f_#{i}")
				end
				copy_var(rhs, var.ref, type) if var
		end
	end
	
	# Values constructed into var are rvalue objects (an unique object with no other references)
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
				
				nodes.reverse.each do |e|
					next unless e.is_a?(AST::VariableDecl)
					ref = @decl_map[e]
					pop_var(ref)
					destroy_var(ref.ref, e.gen)
				end
			when AST::ActionCall
				ptr = new_var
				convert(ast.obj, ptr)
				ref = gen.ref_action(ast.gen, @map, ast.action_type, false)
				o "#{ref}(#{ptr.ref});"
				del_var ptr
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::VariableDecl
				ref = "v_#{ast.var.name}"
				rvar = RealVar.new(ref, ast.gen)
				@decl_map[ast] = rvar
				if ast.value
					convert(ast.value, rvar)
				else
					direct_call(nil, ref(Core::Defaultable::Construct, {Core::Defaultable::T => ast.gen}), nil, ["&#{ref}"], Core::Unit.ctype.type)
				end
				@current_vars.push(rvar)
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::Literal
				assign_var(var, ast.gen, case ast.type
					when :int
						direct_call(var, ref(Core::IntLiteral::Create, {Core::IntLiteral::T => ast.gen}), nil, [ast.value.to_s], ast.gen)
						nil
					when :bool
						ast.value.to_s
					when :string
						direct_call(var, ref(Core::StringLiteral::Create, {Core::StringLiteral::T => ast.gen}), nil, ["(_char *)#{ast.value.inspect}", "#{ast.value.size}"], ast.gen)
						nil
					else
						raise "Unknown literal type #{ast.type}"
				end)
			when AST::UnaryOp
				case ast.op
					when '*'
						ptr = new_var
						convert(ast.node, ptr)
						assign_var(var, ast.gen, "*#{ptr.ref}", true)
						del_var ptr
					when '&'
						single, lval, tvar = lvalue(ast.node)
						assign_var(var, ast.gen, "&(#{lval})", true)
						del_var tvar
				end
			when AST::Field
				case ast.gen[:type]
					when :single
						assign_f(var, ast.gen[:result], ast.gen[:ref], ast.gen[:args].params)
					when :field
						single, lval, lvar = lvalue(ast.obj)
						if ast.gen[:ref].is_a?(AST::Function)
							assign_func(var, lval, ref(ast.gen[:ref], ast.gen[:args].params))
							assign_var(var, ast.gen[:result], nil, true)
						else
							assign_var(var, ast.gen[:result], "(#{lval}).f_#{ast.gen[:ref].name}", true)
						end
						del_var lvar
				end
			when AST::Index
				convert(ast.obj, var)
			when AST::Ref
				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner == @func
					assign_var(var, ast.gen, "v_#{ast.obj.name}", true)
				elsif ast.obj.is_a?(AST::Variable) && ast.obj.declared.owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					assign_var(var, ast.gen.first, "self->f_#{ast.obj.name}", true) # TODO: Check for the case when accesing a field in a parent struct
				else
					assign_f(var, ast.gen.first, ast.obj, ast.gen.last.params)
				end
			when AST::Return # TODO: Ensure all variables in scope get's destroyed on return
				convert(ast.value, RealVar.new("(*result)", nil))
				@current_vars.reverse.each { |v| destroy_var(v.ref, v.type) }
				o "return;"
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::BinOp
				rhs = new_var
				convert(ast.rhs, rhs)
				if ast.op == '='
					assign(var, ast.gen, lvalue(ast.lhs), rhs.ref)
				else
					lhs = new_var
					convert(ast.lhs, lhs)
					
					typeclass = Core::OpMap[ast.op]
					
					if typeclass
						lhs_arg = new_var
						rhs_arg = new_var
						copy_var(lhs.ref, lhs_arg.ref, ast.gen)
						copy_var(rhs.ref, rhs_arg.ref, ast.gen)
						assign_var(lhs_arg, ast.gen, nil)
						assign_var(rhs_arg, ast.gen, nil)
						direct_call(var, ref(typeclass[:func], {typeclass[:param] => ast.gen}), nil, [lhs_arg.ref, rhs_arg.ref], ast.gen)
						del_var rhs_arg, false
						del_var lhs_arg, false
					else
						assign_var(var, ast.gen, lhs.ref + " #{ast.op} " + rhs.ref)
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
				if ast.gen[:call]
					obj = new_var
					convert(ast.obj, obj) 
				end
				
				args = new_var
				ast.args.each_with_index do |a, i|
					arg = new_var
					convert(a, arg)
					o "#{args.ref}.f_#{i} = #{arg.ref};"
					del_var arg, false
				end
				assign_var(args, ast.gen[:args], nil)
				
				if ast.gen[:call]
					direct_call(var, ref(Core::Callable::Apply, {Core::Callable::T => ast.gen[:obj_type]}), obj, [args.ref], ast.gen[:result])
				else
					rvar = var ? var : new_var
					assign_var(rvar, ast.gen[:result], nil)
					direct_call(nil, ref(Core::Constructor::Construct, {Core::Constructor::T => ast.gen[:obj_type]}), nil, ["&#{rvar.ref}", args.ref], Core::Unit.ctype.type)
					del_var rvar unless var
				end
				del_var args, false
				
				del_var obj if ast.gen[:call]
			else
				raise "(unhandled #{ast.class})"
		end
	end
end
