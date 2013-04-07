class Codegen
	def initialize
		@out = {prelude: '', struct_forward: '', struct: '', globals: '', func_forward: '', func: ''}
		@ctx = TypeContext.new(TypeContext::InferArgs.new({}), AST::Variable.new(AST::Src, :ERROR, nil, nil, {}))
		@gen = {}
		@instances = {}
		{AST::Int => 'int', AST::Bool => 'bool', AST::Char => 'char'}.each do |type, name|
			@gen[type] = [TypeContext::InstArgs.new({}, {}, {})]
			type.c_prefix = false
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
		return [[], []] if ast.is_a?(AST::Program)
		params, vars = ast_keys(ast.declared.owner)
		[ast.type_params + params, ast.ctype.type_vars + vars]
	end
	
	def do_ref(ast, old_map, map)
		map = map.dup
		inst_args = old_map.dup
		params, vars = ast_keys(ast)
		
		map.params.each do |k, v|
			if params.include? k
				map.params[k] = @ctx.inst_type(inst_args, v)
			else
				map.params.delete(k)
			end
		end
		params.each { |p| raise "missing key #{p.name}" unless map.params.key?(p) }
		
		map.map.each do |k, v|
			if vars.include? k
				map.map[k] = @ctx.inst_type(inst_args, v)
			else
				map.map.delete(k)
			end
		end
		vars.each { |p| raise "missing var #{p.text}" unless map.map.key?(p) }
		
		puts "ref #{ast.name} old_map:#{old_map} new_map:#{map}"
		gen(ast, map)
		map
	end
	
	def ref(ast, old_map, map)
		mangle(ast, do_ref(ast, old_map, map))
	end
	
	def fixed_type(type, map, &block)
		type = type.prune
		case type
			when Types::Variable
				r = map.map[type]
				raise "Unable to find instance of #{type.text} #{map}" unless r
				fixed_type(r, map, &block)
			when Types::Param
				r = map.params[type.param]
				raise "Unable to find instance of #{type.text} #{map}" unless r
				fixed_type(r, map, &block)
			when Types::Complex
				new_map = do_ref(type.complex, map, TypeContext::InstArgs.new({}, {}, type.args))
				block.(type.complex, new_map)
			else
				raise "(fixed_type unknown #{type.class.inspect} )"
		end
	end

	def mangle_type(type, map)
		fixed_type(type, map) do |complex, map|
			mangle_impl(complex, map)
		end
	end

	def c_type(type, map)
		fixed_type(type, map) do |complex, map|
			case complex
				when AST::Ptr::Node
					"#{c_type(map.params[AST::Ptr::Type], map)}*"
				else
					mangle(complex, map)
			end
		end
	end

	def mangle_name(ast, map)
		r = ast.name.to_s.gsub('_', '_u')
		unless ast.type_params.empty?
			r << "_T#{ast.type_params.map { |p| mangle_type(map.params[p], map) }.join("_l")}_d"
		end
		#puts "mangling #{ast.name} #{ast.ctype.type_vars.map { |p| p.text }.join(",")} #{map}"
		unless ast.ctype.type_vars.empty?
			r << "_V#{ast.ctype.type_vars.map { |p| mangle_type(map.map[p], map) }.join("_l")}_d"
		end
		r
	end

	def mangle_impl(ast, map)
		case ast
			when AST::Ptr::Node
				"_P#{mangle_type(map.params[AST::Ptr::Type], map)}"
			when AST::Func::Node
				args = map.params[AST::Func::Args].tuple_map
				result = map.params[AST::Func::Result]
				"_F#{args.map {|a| mangle_type(a, map) }.join("_l")}_R#{mangle_type(result, map)}"
			else
				owner = ast.declared.owner
				if owner.is_a?(AST::Program)
					mangle_name(ast, map)
				else
					"#{mangle_impl(owner, map)}__#{mangle_name(ast, map)}"
				end
		end
	end
	
	def mangle(ast, map, prefix = true)
		r = "_" + mangle_impl(ast, map)
		#puts "mangling #{ast.name} #{print_map map} => #{r}"
		r
	end
	
	def function_proto(ast, map, bare = false)
		result = ast.ctype.type.args[AST::Func::Result]
		result_type = "void"
		result_type = c_type(result, map) if bare && result != AST::Unit.ctype.type
		o = "#{bare ? 'extern "C" ' : "static "}#{result_type} #{bare ? ast.name : mangle(ast, map)}("
		
		param_types = ast.ctype.type.args[AST::Func::Args].tuple_map
		
		args = ast.params.each_with_index.map { |p, i| "#{c_type(param_types[i], map)} v_#{p.name}" }
		
		unless bare
			args.unshift("#{c_type(result, map)} *result")
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
			when AST::Ptr::Node
			when AST::Func::Node
				args = map.params[AST::Func::Args].tuple_map
				result = map.params[AST::Func::Result]
				name = mangle(ast, map)
				o = "struct #{name}"
				@out[:struct_forward] << o << ";\n"
				o << "\n{\n"
				o << "   void *data;\n"
				o << "   void (*func)(#{(["void *", c_type(result, map) + " *"] + args.map{|a| c_type(a, map)}).join(", ")});\n"
				o << "};\n\n"
				@out[:struct] << o
			when AST::Function
				puts "generating function #{ast.name} #{map}"
				o = function_proto(ast, map)
				@out[:func_forward] << o << ";\n"
				o << "\n{\n"
				(o << "    auto self = (#{c_type(ast.declared.owner.ctype.type, map)} *)data;\n") if ast.declared.owner.is_a?(AST::Complex) && !ast.props[:shared]
				ast.scope.names.values.each do |value|
					next if !value.is_a?(AST::Variable)
					next if ast.params.map(&:var).include?(value)
					o << "    #{c_type(ast.ctype.vars[value], map)} v_#{value.name};\n"
				end
				
				if ast.props[:import]
					o << "    #{"*result = " if ast.ctype.type.args[AST::Func::Result] != AST::Unit.ctype.type}#{ast.name}(#{ast.params.map { |p| "v_#{p.name}"}.join(", ")});"
					@out[:func_forward] << function_proto(ast, map, true) << ";\n"
				else
					o << FuncCodegen.new(self, ast, map).process
				end
				
				o << "\n}\n\n"
				if ast.props[:export]
					o << function_proto(ast, map, true)
					o << "\n{\n    #{c_type(ast.ctype.type.args[AST::Func::Result], map)} result;\n"
					o << "    #{mangle(ast, map)}(#{(["0", "&result"] + ast.params.map(&:name)).join(", ")});"
					(o << "\n    return result;") if ast.ctype.type.args[AST::Func::Result] != AST::Unit.ctype.type
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
					o << "    #{c_type(value.ctype.type, map)} f_#{value.name};\n"
				end
				o << "};\n\n"
				@out[:struct] << o
			when AST::Variable
				return if ast.declared.owner.is_a? AST::Function
				return if ast.declared.owner.is_a?(AST::Struct) && !ast.props[:shared]
				puts "var #{ast.name}"
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
				return if !ast.type_params.empty? || !ast.ctype.type.fixed_type? || !ast.props[:export]
				gen(ast, TypeContext::InstArgs.new({}, {}, {}))
		end
	end
	
	def codegen(ast)
		pass(ast)
		@out.values.join("\n")
	end
end
