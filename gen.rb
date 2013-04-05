class Codegen
	def initialize
		@protos = ''
		@protos = ''
		@ctx = TypeContext.new(TypeContext::InferArgs.new({}), AST::Variable.new(AST::Src, :ERROR, nil, nil, {}))
		@output = "#include <stdbool.h>\n"
		@gen = {}
		@instances = {}
		{AST::Int => 'int', AST::Bool => 'bool', AST::Char => 'char'}.each do |type, name|
			@gen[type] = [{}]
			@output << "typedef #{name} #{mangle(type, {})};\n"
		end
	end
	
	def proto?
		@prototype
	end
	
	def out(*args)
		@output << args.join('')
	end
	
	def ref(ast, old_map, map)
		map = map.dup
		inst_args = TypeContext::InstArgs.new(old_map.dup, {}, old_map.dup)
		map.each { |k, v| map[k] = @ctx.inst_type(inst_args, v) }
		puts "ref #{ast.name} old_map:#{print_map old_map} new_map:#{print_map map}"
		gen(ast, map)
		mangle(ast, map)
	end
	
	def c_type(type, map)
		type = type.prune
		case type
			when Types::Variable, Types::Param
				r = map[type]
				raise "Unable to find instance of #{type.text} #{print_map map}" unless r
				c_type(r, map)
			when Types::Complex
				ref(type.complex, map, type.args)
			when Types::Ptr
				c_type(type.type, map) + "*"
			else
				raise "(c_type unknown #{type.class.inspect} )"
		end
	end

	def ident_str(str)
		str.lines.map { |l| "    " + l }.join
	end

	def gen_body_impl(ast, map, indent = 0)
		gen_body = proc { |a| gen_body_impl(a, map, indent + 1) }
		apply = proc do |list| list.map { |ast| gen_body.(ast) } end
		ind = proc { ("  " * indent) }
		
		case ast
			when AST::Scope
				ident_str apply.(ast.nodes).map { |e| ind.() + e + ";" }.join("\n")
			when AST::Literal
				case ast.type
					when :int, :bool
						ast.value.to_s
					when :string
						ast.value.inspect
					else
						raise "Unknown literal type #{ast.type}"
				end
				
			when AST::Field
				case ast.gen.first
					when :single
						ref(ast.gen[1], map, map)
					when :field
						"#{gen_body.(ast.obj)}.#{ast.name}"
				end
			when AST::Ref
				puts "ref for #{ast.obj.name}, #{ast.gen.first.text} ||| #{ast.gen.last}      map:#{print_map(map)}\n#{ast.source.format}"
				ref(ast.obj, map, ast.gen.last.merged)
			when AST::Return
				"return #{gen_body.(ast.value)}"
			when AST::BinOp
				"(#{gen_body.(ast.lhs)} #{ast.op} #{gen_body.(ast.rhs)})"
			when AST::If
				result = "if(#{gen_body(ast.condition)})\n#{ind.()}{\n" + gen_body.(ast.group) + "\n#{ind.()}}"
				result << "\nelse\n#{ind.()}{\n" + gen_body.(ast.else_node) + "\n#{ind.()}}" if ast.else_node
			when AST::Call
				"#{gen_body.(ast.obj)}(#{apply.(ast.args).join(", ")})"
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end
	
	def print_map(map)
		"[#{map.each.map { |p| "#{p.first.is_a?(AST::TypeParam) ? p.first.name : p.first.text} => #{p.last.text}" }.join(", ")}]"
	end

	def mangle_name(ast, map)
		r = ast.name.to_s.gsub('_', '_u')
		if ast.type_params.size > 0
			r << "_p#{ast.type_params.size}_#{ast.type_params.map { |p| "_" + c_type(map[p], map) }.join("_")}"
		end
		puts "mangling #{ast.name} #{ast.ctype.type_vars.map { |p| p.text }.join(",")} [#{map.each.map { |p| "#{p.first.is_a?(AST::TypeParam) ? p.first.name : p.first.text} => #{p.last.text}" }.join(", ")}]"
		unless ast.ctype.type_vars.empty?
			r << "_v#{ast.ctype.type_vars.size}_#{ast.ctype.type_vars.map { |p| "_" + c_type(map[p], map) }.join("_")}"
		end
		r
	end

	def mangle_impl(ast, map)
		owner = ast.declared.owner
		if owner.is_a?(AST::Program)
			mangle_name(ast, map)
		else
			"#{mangle_impl(owner, map)}__#{mangle_name(ast, map)}"
		end
	end
	
	def mangle(ast, map)
		puts "mangling #{ast.name} #{print_map map}"
		"_" + mangle_impl(ast, map)
	end
	
	def function_proto(ast, map)
		o = "#{c_type(ast.ctype.type.result, map)} #{mangle(ast, map)}("
		
		param_types = ast.ctype.type.args.tuple_map
		
		o << ast.params.each_with_index.map { |p, i| "#{c_type(param_types[i], map)} #{p.name}" }.join(", ") << ")"
	end
	
	def gen(ast, map)
		list = (@gen[ast] ||= [])
		return if list.find { |i| map == i }
		list << map
		
		@output << case ast
			when AST::Function
				puts "generating function #{ast.name} #{print_map map}"
				o = function_proto(ast, map)
				@protos << o << ";\n"
				o << "\n{\n"
				o << gen_body_impl(ast.scope, map, 0)
				o << "\n}"
			when AST::Struct
				o = "struct #{mangle(ast, map)}"
				@protos << o << ";\n"
				o << "\n{\n"
				o << "\n}"
			when AST::Variable
				return if ast.declared.owner.is_a? AST::Function
				return if ast.declared.owner.is_a?(AST::Struct) && !ast.props[:shared]
				o = "#{c_type(ast.ctype.type, map)} #{mangle(ast, map)}"
				@protos << "extern " << o << ";\n"
				o << ";\n"
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
				gen(ast, {})
		end
	end
	
	def codegen(ast)
		pass(ast)
		@output
	end
end
