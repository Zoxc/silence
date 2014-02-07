class Codegen
	def initialize
		@out = {prelude: "#include <cstdint>\n\n", struct_forward: '', struct: '', globals: '', func_forward: '', func: ''}
		@gen = {}
		@named = {}
		@names = 0
		
		{Core::Int => 'intptr_t', Core::UInt => 'uintptr_t', Core::CInt => 'int', Core::Char => 'unsigned char'}.each do |type, name|
			@gen[type] = [TypeContext::Map.new({}, {})]
			@out[:prelude] << "typedef #{name} #{mangle(type, TypeContext::Map.new({}, {}))};\n"
		end
	end
	
	def proto?
		@prototype
	end
	
	def out(*args)
		@output << args.join('')
	end
	
	def find_instance(tc, map, ast)
		typeclass = inst_type(tc, map)
		inst, inst_map = TypeContext.find_instance(nil, nil, typeclass.ref, typeclass.args)
		raise CompileError.new("Unable to find an instance of the type class '#{typeclass.text} for #{ast.scoped_name}\n#{ast.source.format}") unless inst

		ref = inst.scope.names[ast.name]
		raise "Didn't find name '#{ast.name}' in typeclass instance" unless ref

		return ref, inst_map
	end
	
	def inst_type(type, map)
		inst_args = TypeContext::InstArgs.new(proc { |s| s },
			proc { |t|
				r = map.vars[t]
				raise "Unable to find type for #{t}\n#{t.stack} in #{map}\n#{t.source.format}" unless r
				r.prune
			},
			proc { |t|
				if t.param
					r = map.params[t.param]
					raise "Unable to find instance of #{t.ref.scoped_name} in #{map}" unless r
					r.prune
				end})

		TypeContext.inst_type(type, inst_args)
	end
	
	def map_vars(ref, map)
		ctx = TypeContext.new(nil)
		inst_args, inst_map = ctx.inst_map(Core.src, ref, map.params)
		map.vars = Hash[ref.ctype.dependent_vars.map { |var| [var, ctx.inst_type(inst_args, var)] }]
		nil while ctx.reduce(ref)
		
		unless ctx.limits.empty?
			puts "limits for #{ref.scoped_name}"
			ctx.limits.each{|i| puts "    - #{i}"}
			ctx.levels.each{|i| puts "    - #{i}"}
			raise "Unable to reduce vars for map #{map}"  
		end
	end
	
	def format_params(params)
		params.each.map { |k, v| "#{k.scoped_name}: #{v.text}" }.join(", ")
	end

	def do_ref(ast, new_params, q = true)
		map = TypeContext::Map.new({}, {})
		
		AST.type_params(ast).each do |p|
			type = new_params[p]
			raise "missing key #{p.name} in [#{format_params(new_params)}] for #{ast.scoped_name}" unless type
			map.params[p] = type
		end
		
		map_vars(ast, map)
		
		#puts "ref:#{ast.scoped_name} params:(#{format_params(new_params)}) new:#{map}" unless q
			
		gen(ast, map)
		[ast, map, false]
	end
	
	def ref_action(type, map, action_type, opt = true)
		complex, map = fixed_type(type, map)
		raise "Copying non-copyable #{complex.scoped_name} in #{@current.scoped_name}!" if complex.level != :copyable && action_type == :copy
		action = complex.actions[action_type]
		if action
			gen(action, map)
			mangle(action, map)
		else
			raise "Unable to find action #{action_type.inspect} in type #{type.text}" unless opt
		end
	end
	
	def ref(ast, new_params)
		ast, map = do_ref(ast, new_params, false)
		mangle(ast, map)
	end
	
	def fixed_type(type, map)
		type = inst_type(type, map)
		if !type.plain
			[type.ref, TypeContext::Map.new({}, type.args), true] # TODO: Handle the case with a struct inside another with type arguments. 'map' still needs to contain the type argument for the parent
		else
			do_ref(type.ref, type.args)
		end
	end

	def mangle_type(type, map)
		inst_type = inst_type(type, map)
		return "_V#{inst_type.value}" if inst_type.is_a?(Types::Value)

		ast, map, plain = fixed_type(type, map)
		check_map map
		if plain
			owner = ast.declared.owner
			name = mangle_name(ast, nil)
			return (if owner.is_a?(AST::Program)
				"_R#{name}_l"
			else
				"_R#{mangle_impl(owner, map)}__#{name}_l"
			end)
		else
			mangle_impl(ast, map)
		end
	end

	def c_type(type, map)
		complex, map = fixed_type(type, map)
		case complex
			when Core::Ptr::Node
				"#{c_type(map.params[Core::Ptr::Type], map)}*"
			else
				gen(complex, map)
				mangle(complex, map)
		end
	end

	def mangle_name(ast, map)
		r = if ast.is_a?(AST::Function) && ast.action_type
			"_A#{ast.action_type}"
		else
			ast.name.to_s.gsub('_', '_u')
		end
		return r if (ast.is_a?(AST::TypeClass) || !map)
		unless ast.type_params.empty?
			r << "_T#{ast.type_params.map { |p| mangle_type(map.params[p], map) }.join("_n")}_l"
		end
		r
	end

	def check_map(map)
		raise "Got #{map.class} as map" unless map.kind_of?(TypeContext::Map)
	end

	def mangle_impl(ast, map)
		check_map map

		case ast
			when AST::TypeClassInstance
				id = @named[ast] ||= (@names += 1)
				r = "_C#{mangle_impl(ast.typeclass.obj, map)}_I#{id}"
				unless ast.type_params.empty?
					r << "_T#{ast.type_params.map { |p| mangle_type(map.params[p], map) }.join("_n")}"
				end
				r << "_l"
			when Core::Ptr::Node
				"_P#{mangle_type(map.params[Core::Ptr::Type], map)}_l"
			when Core::Func::Node
				args = map.params[Core::Func::Args].tuple_map
				result = map.params[Core::Func::Result]
				"_F#{args.map {|a| mangle_type(a, map) }.join("_n")}_R#{mangle_type(result, map)}_l"
			when Core::Cell::Node
				fields = [map.params[Core::Cell::Val]] + map.params[Core::Cell::Next].tuple_map
				"_Q#{fields.map {|f| mangle_type(f, map) }.join("_n")}_l"
			when AST::Lambda
				"#{mangle_impl(ast.fowner, map)}___L#{ast.__id__}"
			else
				owner = ast.declared.owner
				name = mangle_name(ast, map)
				if owner.is_a?(AST::Program)
					name
				else
					"#{mangle_impl(owner, map)}__#{name}"
				end
		end
	end
	
	def mangle(ast, map, prefix = true)
		"_" + mangle_impl(ast, map)
	end

	def idx(i)
		case i
			when 0
				"f_val"
			else
				"f_next.#{idx(i - 1)}"
		end
	end
	
	def function_proto(ast, map, bare = false, type = ast.ctype.type)
		type = inst_type(type, map)
		result = type.args[Core::Func::Result]
		result_type = "void"
		result_type = c_type(result, map) if bare && result != Core::Unit.ctype.type
		prefix = if bare
				r = 'extern "C" '
				r << "__stdcall " if ast.props[:attrs].include?(:stdcall)
				r
			else
				"static "
			end
		o = "#{prefix}#{result_type} #{bare ? ast.name : mangle(ast, map)}("
		
		param_types = type.args[Core::Func::Args].tuple_map
		
		args = if ast.is_a?(AST::Function) && ast.var_arg
				param_types.each_with_index.map { |p, i| "#{c_type(p, map)} va_#{i}" }
			else
				ast.params.each_with_index.map { |p, i| "#{c_type(param_types[i], map)} v_#{p.name}" }
			end
			
		unless bare
			args.unshift("#{c_type(result, map)} *result") if !ast.is_a?(AST::Function) || !ast.action_type
			args.unshift("void *data") 
		end
		
		o << args.join(", ") << ")"
	end
	
	def gen(ast, map)
		list = (@gen[ast] ||= [])
		return if list.find { |i| map == i }
		@current = ast
		list << map
		Silence.puts "Generating #{ast.scoped_name} - #{map}"
		case ast
			when AST::TypeClass
			when Core::IntLiterals[:num_not][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n    auto &v_self = *(#{c_type(ast.ctype.vars[ast.self], map)} *)data;\n"
				@out[:func] << o << "    *result = -v_self;\n}\n\n"
			when Core::IntLiterals[:bits_other][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n    "
				case ast.name
					when :neg
						o << "auto &v_self = *(#{c_type(ast.ctype.vars[ast.self], map)} *)data;\n"
						o << "    *result = ~v_self;"
					when :shl
						o << "*result = v_lhs << v_rhs;"
					when :shr
						o << "*result = v_lhs << v_rhs;"
				end
				@out[:func] << o << "\n}\n\n"
			when Core::IntLiterals[:bits][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				op = case ast.name
						when :xor
							'^'
						when :and
							'&'
						when :or
							'|'
					end
				@out[:func] << o << "\n{\n    *result = v_lhs #{op} v_rhs;\n}\n\n"
			when Core::IntLiterals[:num][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				op = case ast.name
						when :add
							'+'
						when :sub
							'-'
						when :mul
							'*'
						when :div
							'/'
						when :mod
							'%'
					end
				@out[:func] << o << "\n{\n    *result = v_lhs #{op} v_rhs;\n}\n\n"
			when Core::IntLiterals[:ord][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *result = v_gt > v_ls ? Enum_Order_Greater : (v_gt == v_ls ? Enum_Order_Equal : Enum_Order_Lesser);\n}\n\n"
			when Core::IntLiterals[:eq][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *result = v_lhs == v_rhs ? Enum_bool_true : Enum_bool_false;\n}\n\n"
			when Core::IntLiterals[:default][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *v_obj = 0;\n}\n\n"
			when Core::IntLiterals[:create][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *result = v_i;\n}\n\n"
			when Core::Table::Ref
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				o << "    auto &v_self = *(#{c_type(ast.ctype.vars[ast.self], map)} *)data;\n"
				o << "    *result = &v_self.array[v_index.f_val];\n}\n\n"
				@out[:func] << o
			when Core::Table::Node
				name = mangle(ast, map)

				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				o << "    #{c_type(map.params[Core::Table::Type], map)} array[#{map.params[Core::Table::Size].value}];\n"
				o << "};\n\n"
				@out[:struct] << o
			when Core::FuncEq
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				o << "    *result = v_lhs.data == v_rhs.data && v_lhs.func == v_rhs.func ? Enum_bool_true : Enum_bool_false;\n}\n\n"
				@out[:func] << o
			when Core::SizeOf
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				type = map.params[Core::SizeOfT]
				
				o << "    *result = sizeof(#{c_type(type, map)});\n}\n\n"
				
				@out[:func] << o
			when Core::ForceCast
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				in_type = map.params[Core::ForceCastIn]
				out_type = map.params[Core::ForceCastOut]
				
				o << "    *result = (#{c_type(out_type, map)})v_in;\n}\n\n"
				
				@out[:func] << o
			when Core::CallableFuncApply
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				owner = ast.declared.owner
				
				args = map.params[Core::CallableFuncArgs].tuple_map
				
				self_type = inst_type(owner.ctype.typeclass[owner.typeclass.obj.type_params.first], map)
				
				o << "    auto self = (#{c_type(self_type, map)} *)data;\n"
				o << "    self->func(self->data, result#{args.size.times.map { |i|  ", v_args.#{idx i}" }.join});\n}\n\n"
				
				@out[:func] << o
			when Core::Ptr::Node
			when Core::Func::Node
				args = map.params[Core::Func::Args].tuple_map
				result = map.params[Core::Func::Result]
				name = mangle(ast, map)
				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				o << "   void *data;\n"
				o << "   void (*func)(#{(["void *", c_type(result, map) + " *"] + args.map{|a| c_type(a, map) }).join(", ")});\n"
				o << "};\n\n"
				@out[:struct] << o
			when AST::Lambda
				name = mangle(ast, map)
				o = "struct #{name}__type"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				vars = ast.fowner.ctype.vars
				ast.scope.req_vars.each do |v|
					o << "   #{c_type(vars[v], map)} *r_#{v.name};\n"
				end
				o << "};\n\n"
				@out[:struct] << o

				o = function_proto(ast, map, false, ast.gen)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				o << "    auto &ref = *(#{name}__type *)data;\n"
				o << FuncCodegen.new(self, ast, map).process
				o << "\n}\n\n"
				@out[:func] << o
			when AST::Function
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				owner = ast.declared.owner
				
				if owner.is_a?(AST::Complex) && !ast.props[:shared]
					o << "    auto &v_self = *(#{c_type(ast.ctype.vars[ast.self], map)} *)data;\n"
				end

				# TODO: Generate an error when using type with copy operators in import/export functions

				if [:create, :create_args].include?(ast.action_type) && owner.is_a?(AST::StructCase)
					o << "    v_self.type = #{owner.parent.cases.index(owner)};\n"
				end
				
				if ast.props[:import]
					o << "    #{"*result = " if ast.ctype.type.args[Core::Func::Result] != Core::Unit.ctype.type}#{ast.name}(#{ast.params.map { |p| "v_#{p.name}"}.join(", ")});"
					@out[:func_forward] << function_proto(ast, map, true) << ";\n"
				else
					o << FuncCodegen.new(self, ast, map).process
				end
				
				o << "\n}\n\n"
				if ast.props[:export]
					o << function_proto(ast, map, true)
					o << "\n{\n    #{c_type(ast.ctype.type.args[Core::Func::Result], map)} result;\n"
					o << "    #{mangle(ast, map)}(#{(["0", "&result"] + ast.params.map { |p| "v_#{p.name}"}).join(", ")});"
					(o << "\n    return result;") if ast.ctype.type.args[Core::Func::Result] != Core::Unit.ctype.type
					o << "\n}\n\n"
				end
				@out[:func] << o
			when AST::EnumValue
				owner = mangle(ast.owner, map)
				@out[:globals] << "static #{owner} #{mangle(ast, map)} = Enum#{owner}_#{ast.name};\n"
			when AST::Enum
				name = mangle(ast, map)
				o = "enum #{name}\n{\n"
				ast.values.each do |v|
					o << "    Enum#{name}_#{v.name},\n"
				end
				@out[:struct_forward] << o << "};\n\n"
			when AST::Struct
				name = mangle(ast, map)
				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"

				o << "\n{\n"

				if ast.level != :copyable && !ast.is_a?(AST::StructCase)
					o << "#{name}() = default;\n\n"
					o << "#{name}(const #{name}&) = delete;\n\n" 
					o << "#{name}& operator=(const #{name}&) = delete;\n\n" 
				end

				o << "    _uint type;\n" if ast.enum?
				
				ast.scope.names.values.each do |value|
					next if !value.is_a?(AST::Variable) || value.props[:shared]
					field_map = map.copy
					map_vars(value, field_map)
					o << "    #{c_type(value.ctype.type, field_map)} f_#{value.name};\n"
				end

				if ast.enum?
					o << "    union {\n"
					ast.cases.each_with_index do |c, i|
						gen(c, map)
						o << "        #{mangle(c, map)} e_#{i};\n"
					end
					o << "    };\n"
				end

				o << "};\n\n"
				@out[:struct] << o

			when AST::Variable
				return if ast.declared.owner.is_a? AST::Function
				return if ast.declared.owner.is_a?(AST::Struct) && !ast.props[:shared]
				name = mangle(ast, map)
				o = "#{"static " unless ast.props[:export]}#{c_type(ast.ctype.type, map)} #{name};\n"
				@out[:globals] << o
				if ast.decl.value
					cn = "#{name}_Init"
					code = FuncCodegen.new(self, ast, map).global
					o = "struct #{cn} {\n#{cn}()\n{\n#{code}\n}\n};\nstatic #{cn} #{cn}_var;\n\n"
					@out[:func] << o
				end
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end

	def pass(ast)
		apply = proc do |nodes| nodes.each { |ast| pass(ast) } end
		
		case ast
			when AST::Program
				pass(ast.scope)
			when AST::GlobalScope
				apply.(ast.nodes)
			when AST::Function
				return if !ast.type_params.empty? || !ast.ctype.type.fixed_type? || !ast.props[:export]
				gen(ast, TypeContext::Map.new({}, {}))
		end
	end
	
	def codegen(ast)
		pass(ast)
		@out.values.join("\n")
	end
end
