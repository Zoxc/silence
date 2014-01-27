class FuncCodegen
	attr_reader :gen, :func, :map
	
	def initialize(gen, func, map)
		@gen = gen
		@func = func
		@out = []
		@map = map
		@var_name = 0
		@label_name = 0
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

	def fowner
		@func.fowner
	end

	def new_label
		"L#{@label_name += 1}"
	end

	def gen_label(l)
		@out << "#{l}:;"
	end

	def new_var(&block)
		var = Var.new(@var_name += 1, nil)
		@vars << var
		@current_vars.push(var)
		var
	end

	def copy_var(src, dst, type)
		ref = gen.ref_action(type, @map, :copy)
		if dst != src
			o "#{dst} = #{src}; #{"#{ref}(&(#{dst}));" if ref} // copy"
		else
			o "#{"#{ref}(&(#{dst}));" if ref} // copy #{dst}"
		end
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
	
	def content
		raise "Undestroyed variables:\n#{@current_vars.map(&:ref).join("\n")}" unless @current_vars.empty?

		@vars.map { |v| v.decl(self) }.join + "\n" + @out.join("\n")
	end

	def global
		n = ref(@func, @map.params)
		var = RealVar.new(ref(@func, @map.params), @func.ctype.type)

		convert(@func.decl.value, var)

		content
	end

	def process
		ftype = case @func
			when AST::Function
				@func.ctype.type
			when AST::Lambda
				@func.gen
		end
		params = ftype.args[Core::Func::Args].tuple_map.zip(@func.params)
		params = params.map do |type, p|
			var = RealVar.new("v_#{p.name}", type)
			@current_vars.push(var)
			var
		end

		handle_action = proc do
			owner = @func.declared.owner

			code = []

			owner.scope.names.values.each do |value|
				next if !value.is_a?(AST::Variable) || value.props[:shared]
				field_map = map.copy
				@gen.map_vars(value, field_map)

				ref = @gen.ref_action(value.ctype.type, field_map, @func.action_type)

				code << ["    #{"#{ref}(#{ref_field(value)});" if ref} // #{@func.action_type} #{value.name}\n"]
			end

			if owner.enum?

				out = "    switch(#{self_ref}->type) {\n"
				owner.cases.each_with_index do |w, i|
					action = w.actions[@func.action_type]
					if action
						@gen.gen(action, @map)
						ref = @gen.mangle(action, @map)
					end

					out << "        case #{i}:\n"
					out << "                   #{"#{ref}(#{self_ref});" if ref} // #{@func.action_type} case #{w.name}\n"
					out << "                   break;\n"
				end
				out << "    }\n"

				code << out
			end
			code = code.reverse if @func.action_type == :destroy
			code.unshift "\n    // #{@func.action_type} fields\n"
			@out << code.join
		end

		if @func.is_a?(AST::Function)
			(@func.gen_init_list || []).each do |gen|
				direct_call(nil, ref(Core::Defaultable::Construct, {Core::Defaultable::T => gen[:type]}), nil, [ref_field(gen[:field])], Core::Unit.ctype.type)
			end

			@func.init_list.each do |ast|
				var = new_var
				convert(ast.expr, var)
				o "*(#{ref_field(ast.gen[:ref])}) = #{var.ref}; // construct"
				del_var var, false
			end

			handle_action.() if @func.action_type == :copy
		end

		convert(@func.scope, nil)

		handle_action.() if @func.is_a?(AST::Function) && @func.action_type == :destroy 

		params.reverse.each do |var|
			pop_var(var)
			destroy_var(var.ref, var.type)
		end
		
		content
	end

	def idx(i)
		@gen.idx(i)
	end

	def map_ref(obj, params)
		map = {}
		
		params.each do |k, v|
			map[k] = @gen.inst_type(v, @map)
		end if params
		
		owner = obj.declared.owner
		
		if owner.is_a?(AST::TypeClass)
			ref, new_map = @gen.find_instance(owner.ctype.type, TypeContext::Map.new({}, map), obj)
			
			ref.type_params.each_with_index do |p, i|
				param = obj.type_params[i]
				raise "Didn't find matching param for '#{p.scoped_name} for typeclass object #{obj.scoped_name}'" unless param
				param_mapped = map.params[param]
				raise "Param '#{p.name}' type not provided" unless param_mapped
				new_map.params[p] = param_mapped
			end
			
			obj = ref
			map = new_map
		end

		return obj, map
	end
	
	def ref(obj, params)
		obj, map = map_ref(obj, params)
		@gen.ref(obj, map)
	end
	
	def o(str)
		@out << ("    " + str)
	end
	
	def gen_func(data, ref, type)
		var = new_var
		o var.ref + ".func = " + ref + ";"
		(o var.ref + ".data = " + data + ";") if data
		assign_var(var, type, nil)
		del_var var
		return "&#{var.ref}"
	end
	
	def make_ptr(type)
		InferContext.make_ptr(Core.src, type)
	end

	def func_data(obj)
		if fowner.declared.owner == obj.declared.owner && !obj.props[:shared]
			self_ref
		else
			"nullptr"
		end
	end

	def assign_f_impl(type, obj, params)
		obj, map = map_ref(obj, params)

		if obj.is_a?(AST::TypeParam)
			# TODO: This should reference read-only memory instead of constructing a new value
			tvar = new_var
			direct_call(tvar, ref(Core::IntLiteral::Create, {Core::IntLiteral::T => type}), nil, [map[obj].value.to_s], type)
			assign_var(tvar, type, nil)
			del_var tvar
			return "&#{tvar.ref}"
		end

		ref = @gen.ref(obj, map)

		if obj.is_a?(AST::Function)
			gen_func(func_data(obj), ref, type)
		else
			"&#{ref}"
		end
	end

	def assign_f(var, type, obj, params)
		assign_var(var, make_ptr(type), assign_f_impl(type, obj, params), true)
	end

	def readonly(ast, var)
		case ast
			when AST::Call
				return readonly(ast.args.first, var) if ast.gen[:type] == :binding

			when AST::UnaryOp
				if ast.op == '*'
					lvalue(ast, var, false)
					return
				end
			when AST::Field, AST::Ref
				lvalue(ast, var, false)
				return
		end

		tvar = new_var
		convert(ast, tvar)
		assign_var(var, make_ptr(tvar.type), "&#{tvar.ref}")
		tvar
	end

	def extract_func_f(obj, params)
		if obj.is_a?(AST::Function)
			[ref(obj, params), func_data(obj)]
		end
	end

	def extract_func(ast)
		case ast
			when AST::Field
				case ast.gen[:type]
					when :single
						extract_func_f(ast.gen[:ref], ast.gen[:args].params)
					when :field
						if ast.gen[:ref].is_a?(AST::Function)
							obj_ptr = new_var
							del_var obj_ptr, false
							ovar = readonly(ast.obj, obj_ptr)
							[ref(ast.gen[:ref], ast.gen[:args].params), obj_ptr.ref, ovar]
						end
				end
			when AST::Ref
				owner = ast.obj.declared.owner

				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.inside?(@func.fowner.scope)
				elsif ast.obj.is_a?(AST::Variable) && owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
				elsif ast.gen[:type] == :self
				else
					extract_func_f(ast.gen[:ref], ast.gen[:args].params)
				end
		end
	end

	def call_args(ast, var, lval = nil)
		if ast.gen[:type] == :binding
			convert(ast.args.first, var)
			return
		end

		if ast.gen[:type] == :call
			efunc = extract_func(ast.obj)
			if efunc
				ovar = efunc[2]
			else
				obj = new_var
				ovar = readonly(ast.obj, obj) 
			end
		end
		
		unless efunc
			args = new_var
			ast.args.each_with_index do |a, i|
				arg = new_var
				convert(a, arg)
				o "#{args.ref}.#{idx i} = #{arg.ref};"
				del_var arg, false
			end
			assign_var(args, ast.gen[:args], nil)
		end
		
		case ast.gen[:type]
			when :call
				if efunc
					args = ast.args.map do |a, i|
						arg = new_var
						convert(a, arg)
						arg
					end
					direct_call(var, efunc[0], efunc[1], args.map(&:ref), ast.gen[:result])
					args.reverse.each { |arg| del_var(arg, false) }
				else
					direct_call(var, ref(Core::Callable::Apply, {Core::Callable::T => ast.gen[:obj_type]}), obj.ref, [args.ref], ast.gen[:result])
				end

			when :index
				direct_call(var, ref(Core::Indexable::Ref, {Core::Indexable::T => ast.gen[:obj_type]}), lval, [args.ref], ast.gen[:result])
		
			when :construct
				rvar = var ? var : new_var
				assign_var(rvar, ast.gen[:result], nil)
				direct_call(nil, ref(Core::Constructor::Construct, {Core::Constructor::T => ast.gen[:obj_type]}), nil, ["&#{rvar.ref}", args.ref], Core::Unit.ctype.type)
				del_var rvar unless var
			else
				raise "(unhandled)"
		end

		
		del_var(args, false) unless efunc
		del_var ovar if ovar
		del_var obj if obj
	end

	def self_ref
		return unless @func.fowner.self
		if @func != @func.fowner
			"ref.r_self"
		else
			"(&v_self)"
		end
	end

	def ref_field(obj)
		owner = obj.declared.owner
		if owner == fowner.declared.owner && owner.is_a?(AST::StructCase)
			idx = owner.parent.cases.index(owner)
			"&#{self_ref}->e_#{idx}.f_#{obj.name}"
		else
			"&#{self_ref}->f_#{obj.name}" # TODO: Check for the case when accesing a field in a parent struct
		end
	end

	def lvalue(ast, var, proper = true)
		case ast
			when AST::Call
				case ast.gen[:type]
					when :index
						obj_ptr = new_var
						lvalue(ast.obj, obj_ptr)
						call_args(ast, var, obj_ptr.ref)
						del_var obj_ptr
					when :binding
						lvalue(ast.args.first, var, proper)
					else
						raise
				end

			when AST::UnaryOp
				case ast.op
					when '*'
						convert(ast.node, var)
					else
						raise "(unhandled)"
				end
			when AST::Field
				case ast.gen[:type]
					when :single_obj
						# Evalute the obj part. It might have side effects
						obj_ptr = new_var
						if proper
							lvalue(ast.obj, obj_ptr, true)
						else
							ovar = readonly(ast.obj, obj_ptr)
							del_var ovar if ovar
						end
						del_var obj_ptr

						assign_f(var, ast.gen[:result], ast.gen[:ref], ast.gen[:args].params)
					when :single
						assign_f(var, ast.gen[:result], ast.gen[:ref], ast.gen[:args].params)
					when :field
						obj_ptr = new_var
						if proper
							lvalue(ast.obj, obj_ptr, true)
						else
							ovar = readonly(ast.obj, obj_ptr)
						end
						del_var obj_ptr, false

						ast.gen[:deref].each do |deref|
							new_obj_ptr = new_var
							del_var new_obj_ptr, false
							direct_call(new_obj_ptr, ref(Core::Reference::Get, {Core::Reference::T => deref[:type]}), obj_ptr.ref, [], deref[:result])
							obj_ptr = new_obj_ptr
						end

						ref = if ast.gen[:ref].is_a?(AST::Function)
							gen_func(obj_ptr.ref, ref(ast.gen[:ref], ast.gen[:args].params), ast.gen[:result])
						else
							extension = ast.gen[:extension]
							if extension
								idx = extension.parent.cases.index(extension) 
								"&#{obj_ptr.ref}->e_#{idx}.f_#{ast.gen[:ref].name}"
							else
								"&#{obj_ptr.ref}->f_#{ast.gen[:ref].name}"
							end
						end

						assign_var(var, make_ptr(ast.gen[:result]), ref, true)
						del_var ovar if ovar
				end
			when AST::Ref
				owner = ast.obj.declared.owner

				if ast.obj.is_a?(AST::Variable) && ast.obj.declared.inside?(@func.fowner.scope)
					if @func.scope.fscope != ast.obj.declared.fscope
						assign_var(var, make_ptr(ast.gen), "ref.r_#{ast.obj.name}", true)
					else
						assign_var(var, make_ptr(ast.gen), "&v_#{ast.obj.name}", true)
					end
				elsif ast.obj.is_a?(AST::Variable) && owner.is_a?(AST::Complex) && !ast.obj.props[:shared]
					# TODO: Turn this into a AST::Field with self as obj
					assign_var(var, make_ptr(ast.gen[:result]), ref_field(ast.obj), true)
				elsif ast.gen[:type] == :self
					type = @func.fowner.ctype.type
					assign_var(var, make_ptr(type), gen_func(self_ref, @gen.mangle(@func.fowner, @map), type), true)
				else
					# TODO: Also merge the single AST::Field case and this
					assign_f(var, ast.gen[:result], ast.gen[:ref], ast.gen[:args].params)
				end
			else
				raise "(unhandled #{ast.class})"
		end
	end
	
	def direct_call(var, ref, obj, args, result_type)
		rvar = var ? var : new_var
		assign_var(rvar, result_type, nil)
		o "#{ref}(#{obj ? obj : "nullptr"}, &#{rvar.ref}#{args.map{|a| ", #{a}"}.join});"
		del_var rvar unless var
	end
	
	def assign(var, type, lhs, rhs)
		assign_var(var, type, nil) if var

		case lhs
			when AST::Tuple
				tuple_map = type.tuple_map

				lhs.nodes.each_with_index do |v, i|
					assign(nil, tuple_map[i], v, "(#{rhs}).#{idx i}")
				end
				copy_var(rhs, var.ref, type) if var
			else
				ptr = new_var
				dptr = "*#{ptr.ref}"
				lvalue(lhs, ptr)

				destroy_var(dptr, type)
				copy_var(rhs, dptr, type)
				copy_var(dptr, var.ref, type) if var

				del_var ptr
		end
	end

	def lvalue_to_convert(ast, var)
		tvar = new_var
		lvalue(ast, tvar, false)
		type = @gen.inst_type(tvar.type, @map).args.values.first
		assign_var(var, type, "*#{tvar.ref}", true)
		del_var tvar
	end

	def bin_op(ast, lhs, rhs, op, var)
		typeclass = Core::OpMap[op]

		cmp_op = ['>', '<', '>=', '<='].include?(op)
		cmp_op_var = new_var if cmp_op
		
		if typeclass
			lhs_arg = new_var
			rhs_arg = new_var
			copy_var(lhs, lhs_arg.ref, ast.gen[:arg])
			copy_var(rhs.ref, rhs_arg.ref, ast.gen[:arg])
			assign_var(lhs_arg, ast.gen[:arg], nil)
			assign_var(rhs_arg, ast.gen[:arg], nil)
			direct_call(cmp_op ? cmp_op_var : var, ref(typeclass[:func], {typeclass[:param] => ast.gen[:arg]}), nil, [lhs_arg.ref, rhs_arg.ref], cmp_op ? Types::Ref.new(Core.src, Core::Order) : ast.gen[:result])
			del_var rhs_arg, false
			del_var lhs_arg, false
		else
			assign_var(var, ast.gen[:result], lhs + " #{op} " + rhs.ref)
		end

		if cmp_op
			values = case op
				when '>'
					["Greater"]
				when '<'
					["Lesser"]
				when '>='
					["Greater", "Equal"]
				when '<='
					["Lesser", "Equal"]
			end
			r = values.map {|v| "#{cmp_op_var.ref} == Enum_Order_#{v}" }.join(" || ")
			assign_var(var, ast.gen[:result], "(#{r}) ? Enum_bool_true : Enum_bool_false")
			del_var cmp_op_var
		end

		o "#{var.ref} = #{var.ref} == Enum_bool_true ? Enum_bool_false : Enum_bool_true;" if op == '!='
	end
	
	# Values constructed into var are rvalue objects (an unique object with no other references)
	def convert(ast, var)
		case ast
			when AST::ExpressionGroup
				convert(ast.scope, var)
			when AST::Lambda
				ref_n = "v_#{@var_name += 1}"

				@gen.gen(ast, @map)
				o "#{ast.name} #{ref_n};"
				ast.scope.req_vars.each do |v|
					o "#{ref_n}.r_#{v.name} = #{v.declared.fscope == @func.scope ? "&v_#{v.name}" : "ref.r_#{v.name}"};\n"
				end
				o "#{var.ref}.func = #{@gen.mangle(ast, @map)};"
				o "#{var.ref}.data = &#{ref_n};"
				assign_var(var, ast.gen, nil)
			when AST::Scope
				nodes = ast.nodes.compact

				ast.names.values.each do |v|
					next unless v.is_a?(AST::Variable)
					next unless v.decl

					o "#{@gen.c_type(fowner.ctype.vars[v], @map)} v_#{v.name};"
				end
				o ""

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
			when AST::Match
				resume = new_label

				expr = new_var
				convert(ast.expr, expr)

				when_labels = ast.whens.map { new_label }

				ast.whens.each_with_index do |w, i|
					test = new_var
					w_expr = new_var
					convert(w.type, w_expr)
					direct_call(test, ref(Core::Eq::Equal, {Core::Eq::T => ast.gen[:type]}), "&#{expr.ref}", [expr.ref, w_expr.ref], Core::Bool.ctype.type)
					del_var w_expr
					o "    if(#{test.ref} == Enum_bool_true) goto #{when_labels[i]};"
					del_var test
				end

				if ast.else_group
					convert(ast.else_group, ast.gen[:unused] ? nil : var) 
					o "goto #{resume};"
				end

				ast.whens.each_with_index do |w, i|
					gen_label when_labels[i]
					convert(w.group, ast.gen[:unused] ? nil : var)
					o "goto #{resume};"
				end

				gen_label resume
				del_var expr
			when AST::MatchAs
				resume = new_label

				type = @gen.inst_type(ast.gen[:expr], @map) 

				expr = RealVar.new("v_#{ast.rest.binding.name}", type)
				o "#{@gen.c_type(type, @map)} #{expr.name};"

				convert(ast.expr, expr)

				when_labels = ast.rest.whens.map { new_label }

				idx = proc { |i| type.ref.cases.index(ast.gen[:when_objs][i]) }

				o "switch(#{expr.ref}.type) {"
				ast.rest.whens.each_with_index do |w, i|
					o "    case #{idx.(i)}: goto #{when_labels[i]};"
				end
				o "}"

				if ast.rest.else_group
					convert(ast.rest.else_group, ast.gen[:unused] ? nil : var) 
					o "goto #{resume};"
				end

				ast.rest.whens.each_with_index do |w, i|
					gen_label when_labels[i]
					convert(w.group, ast.gen[:unused] ? nil : var)
					o "goto #{resume};"
				end

				gen_label resume
				destroy_var(expr.ref, type)
			when AST::ActionCall
				ptr = new_var
				convert(ast.obj, ptr)

				arg_types = if ast.arg
					arg = new_var
					convert(ast.arg, arg)
					arg.type
				else
					Types::Ref.new(Core.src, Core::Unit)
				end.tuple_map

				args = arg_types.each_with_index.map do |type, i|
					"#{arg.ref}.#{idx i}"
				end

				ref = gen.ref_action(ast.gen[:type], @map, ast.action_type, false)
				o "#{ref}(#{ptr.ref}#{args.map{|a| ", #{a}"}.join});"

				del_var(arg, false) if arg
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
					when :string
						direct_call(var, ref(Core::StringLiteral::Create, {Core::StringLiteral::T => ast.gen}), nil, ["(_char *)#{ast.value.inspect}", "#{ast.value.size}"], ast.gen)
						nil
					else
						raise "Unknown literal type #{ast.type}"
				end)
			when AST::Ref, AST::Field
				lvalue_to_convert(ast, var)
			when AST::UnaryOp
				case ast.op
					when '+'
						convert(ast.node, var)
					when '*'
						lvalue_to_convert(ast, var)
					when '&'
						lvalue(ast.node, var)
					else
						obj = new_var
						ovar = readonly(ast.node, obj)

						map = Core::UnaryOpMap[ast.op]

						direct_call(var, ref(map[:func], {map[:param] => ast.gen}), obj.ref, [], ast.gen)
						
						del_var ovar if ovar
						del_var obj
				end
			when AST::Index
				convert(ast.obj, var)
			when AST::Return # TODO: Ensure all variables in scope get's destroyed on return
				convert(ast.value, RealVar.new("(*result)", nil))
				@current_vars.reverse.each { |v| destroy_var(v.ref, v.type) }
				o "return;"
				assign_var(var, Core::Unit.ctype.type, nil)
			when AST::BinOp
				rhs = new_var
				convert(ast.rhs, rhs)
				if ast.op == '='
					assign(var, ast.gen[:result], ast.lhs, rhs.ref)
				else
					lhs = new_var

					plain_op = ast.gen[:plain_op]

					if plain_op != ast.op
						tmp = new_var
						lvalue(ast.lhs, lhs)
						bin_op(ast, "*#{lhs.ref}", rhs, plain_op, tmp)
						destroy_var("*#{lhs.ref}", ast.gen[:arg])
						copy_var(tmp.ref, "*#{lhs.ref}", ast.gen[:arg])
						copy_var(tmp.ref, var.ref, ast.gen[:result]) if var
						assign_var(var, ast.gen[:result], nil)
						del_var tmp
					else
						convert(ast.lhs, lhs)
						bin_op(ast, lhs.ref, rhs, plain_op, var)
					end

					del_var lhs
				end
				del_var rhs
			when AST::If
				cond = new_var
				el = new_label
				convert(ast.condition, cond)
				o "if(#{cond.ref} == Enum_bool_false)"
				o "    goto #{el};"
				del_var cond
				convert(ast.group, ast.gen ? nil : var)
				if ast.else_node
					ed = new_label
					o "goto #{ed};"
					gen_label el
					convert(ast.else_node, ast.gen ? nil : var)
					gen_label ed
				else
					gen_label el
				end
			when AST::Grouped
				convert(ast.node, var)
			when AST::Call
				call_args(ast, var)
			else
				raise "(unhandled #{ast.class})"
		end
	end
end
