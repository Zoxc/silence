class Codegen
	def initialize
		@out = {prelude: "#include <cstdint>\n\n", struct_forward: '', struct: '', globals: '', func_forward: '', func: ''}
		@gen = {}
		@named = {}
		@names = 0
		
		{Core::Int => 'intptr_t', Core::UInt => 'uintptr_t', Core::Bool => 'bool', Core::Char => 'char'}.each do |type, name|
			@gen[type] = [TypeContext::Map.new({}, {})]
			@out[:prelude] << "typedef #{name} #{mangle(type, {})};\n"
		end
	end
	
	def proto?
		@prototype
	end
	
	def out(*args)
		@output << args.join('')
	end
	
	def ast_keys(ast)
		case ast
			when AST::Program
				[]
			when AST::TypeClassInstance
				ast.kind.params
			else
				ast.kind.params + ast_keys(ast.declared.owner)
		end
	end
	
	def find_instance(tc, map, ast)
		typeclass = inst_type(tc, map)
		inst, inst_map = TypeContext.find_instance(nil, nil, typeclass.complex, typeclass.args)
		raise TypeError.new("Unable to find an instance of the type class '#{typeclass.text} for #{ast.name}\n#{ast.source.format}") unless inst

		ref = inst.scope.names[ast.name]
		raise "Didn't find name '#{ast.name}' in typeclass instance" unless ref

		return ref, inst_map
	end
	
	def map_ref(ref, map)
		if ref.param
			r = map.params[ref.param]
			raise "Unable to find instance of #{ref.scoped_name} #{map}" unless r
			r.prune
		end
	end
	
	def inst_type(type, map)
		type = type.prune
		case type
			when Types::Variable
				r = map.vars[type]
				raise "Unable to find type for #{type.text}" unless r
				r.prune
			when Types::Complex
				ref = map_ref(type, map)
				if type.args.empty?
					ref || type
				else
					ref = ref ? ref.complex : type.complex
					Types::Complex.new(type.source, ref, Hash[type.args.map { |k, v| [k, inst_type(v, map)] }])
				end
			else
				raise "(inst_type unknown #{type.class.inspect})"
		end
	end
	
	def map_vars(ref, map)
		ctx = TypeContext.new(nil)
		inst_args = ctx.inst_map(Core.src, ref, map.params)
		map.vars = Hash[ref.ctype.dependent_vars.map { |var| [var, ctx.inst_type(inst_args, var)] }]
		ctx.reduce(ref)
		
		raise "Unable to reduce vars for map #{map}" unless ctx.limits.empty?
	end
	
	def format_params(params)
		params.each.map { |k, v| "#{k.scoped_name}: #{v.text}" }.join(", ")
	end

	def do_ref(ast, new_params, q = true)
		map = TypeContext::Map.new({}, {})
		
		ast_keys(ast).each do |p|
			type = new_params[p]
			raise "missing key #{p.name} in [#{format_params(new_params)}]" unless type
			map.params[p] = type
		end
		
		map_vars(ast, map)
		
		puts "ref:#{ast.scoped_name} params:(#{format_params(new_params)}) new:#{map}" unless q
			
		gen(ast, map)
		[ast, map]
	end
	
	def ref_action(type, map, action_type, opt = true)
		complex, map = fixed_type(type, map)
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
		do_ref(type.complex, type.args)
	end

	def mangle_type(type, map)
		complex, map = fixed_type(type, map)
		mangle_impl(complex, map)
	end

	def c_type(type, map)
		complex, map = fixed_type(type, map)
		case complex
			when Core::Ptr::Node
				"#{c_type(map.params[Core::Ptr::Type], map)}*"
			else
				mangle(complex, map)
		end
	end

	def mangle_name(ast, map)
		r = if ast.is_a?(AST::Function) && ast.action_type
			"_A#{ast.action_type}"
		else
			ast.name.to_s.gsub('_', '_u')
		end
		return r if ast.is_a? AST::TypeClass
		unless ast.kind.params.empty?
			r << "_T#{ast.kind.params.map { |p| mangle_type(map.params[p], map) }.join("_n")}_l"
		end
		r
	end

	def mangle_impl(ast, map)
		case ast
			when AST::TypeClassInstance
				id = @named[ast] ||= (@names += 1)
				r = "_C#{mangle_impl(ast.typeclass.obj, map)}_I#{id}"
				unless ast.kind.params.empty?
					r << "_T#{ast.kind.params.map { |p| mangle_type(map.params[p], map) }.join("_n")}"
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
	
	def function_proto(ast, map, bare = false)
		result = ast.ctype.type.args[Core::Func::Result]
		result_type = "void"
		result_type = c_type(result, map) if bare && result != Core::Unit.ctype.type
		o = "#{bare ? 'extern "C" ' : "static "}#{result_type} #{bare ? ast.name : mangle(ast, map)}("
		
		param_types = ast.ctype.type.args[Core::Func::Args].tuple_map
		
		args = ast.params.each_with_index.map { |p, i| "#{c_type(param_types[i], map)} v_#{p.name}" }
		
		unless bare
			args.unshift("#{c_type(result, map)} *result") if !ast.action_type
			args.unshift("void *data") 
		end
		
		o << args.join(", ") << ")"
	end
	
	def gen(ast, map)
		list = (@gen[ast] ||= [])
		return if list.find { |i| map == i }
		list << map
		puts "Generating #{ast.name}"
		case ast
			when AST::TypeClass
			when Core::IntLiterals[:default][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *v_obj = 0;\n}\n\n"
			when Core::IntLiterals[:create][ast]
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				@out[:func] << o << "\n{\n    *result = v_input;\n}\n\n"
			when Core::ForceCast
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				in_type = map.params[Core::ForceCastIn]
				out_type = map.params[Core::ForceCastOut]
				
				o << "    *result = reinterpret_cast<#{c_type(out_type, map)}>(v_in);\n}\n\n"
				
				@out[:func] << o
			when Core::CallableFuncApply
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				owner = ast.declared.owner
				
				args = map.params[Core::CallableFuncArgs].tuple_map
				
				self_type = inst_type(owner.ctype.typeclass[owner.typeclass.obj.kind.params.first], map)
				
				o << "    auto self = (#{c_type(self_type, map)} *)data;\n"
				o << "    self->func(self->data, result#{args.size.times.map { |i|  ", v_args.f_#{i}" }.join});\n}\n\n"
				
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
			when Core::Cell::Node
				fields = [map.params[Core::Cell::Val]] + map.params[Core::Cell::Next].tuple_map
				name = mangle(ast, map)
				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				o << fields.each_with_index.map { |f, i| "   #{c_type(f, map)} f_#{i};\n" }.join
				o << "};\n\n"
				@out[:struct] << o
			when AST::Function
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				
				owner = ast.declared.owner
				
				if owner.is_a?(AST::Complex) && !ast.props[:shared]
					if owner.is_a?(AST::TypeClassInstance)
						self_type = inst_type(owner.ctype.typeclass[owner.typeclass.obj.kind.params.first], map)
					else
						self_type = owner.ctype.type
					end
					o << "    auto self = (#{c_type(self_type, map)} *)data;\n"
				end
				ast.scope.names.values.each do |value|
					next if !value.is_a?(AST::Variable)
					next if ast.params.map(&:var).include?(value)
					o << "    #{c_type(ast.ctype.vars[value], map)} v_#{value.name};\n"
				end
				
				# TODO: Generate an error when using type with copy operators in import/export functions
				
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
			when AST::Struct
				name = mangle(ast, map)
				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				ast.scope.names.values.each do |value|
					next if !value.is_a?(AST::Variable) || value.props[:shared]
					field_map = map.copy
					map_vars(value, field_map)
					o << "    #{c_type(value.ctype.type, field_map)} f_#{value.name};\n"
				end
				o << "};\n\n"
				@out[:struct] << o
			when AST::Variable
				return if ast.declared.owner.is_a? AST::Function
				return if ast.declared.owner.is_a?(AST::Struct) && !ast.props[:shared]
				o = "#{"static " unless ast.props[:export]}#{c_type(ast.ctype.type, map)} #{mangle(ast, map)};\n"
				@out[:globals] << o
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
				return if !ast.kind.params.empty? || !ast.ctype.type.fixed_type? || !ast.props[:export]
				gen(ast, TypeContext::Map.new({}, {}))
		end
	end
	
	def codegen(ast)
		pass(ast)
		@out.values.join("\n")
	end
end
