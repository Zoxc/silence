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
	
	def ref(obj, map)
		@gen.ref(obj, @map, map)
	end
	
	def o(str)
		@out << ("    " + str)
	end
	
	def assign_func(var, data, ref)
		o var.ref + ".func = " + ref + ";"
		(o var.ref + ".data = &" + data + ";") if data
	end
	
	def convert(ast, var)
		case ast
			when AST::Scope
				if ast.nodes.empty?
					assign_var(var, [AST::Unit, true], nil)
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
			when AST::Field
				case ast.gen[:type]
					when :single
						assign_var(var, ast.gtype, ref(ast.gen[:ref], ast.gen[:args]))
					when :field
						obj = new_var
						convert(ast.obj, obj)
						
						if ast.gen[:ref].is_a?(AST::Function)
							assign_var(var, ast.gtype, nil)
							assign_func(var, obj.ref, ref(ast.gen[:ref], ast.gen[:args]))
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
					assign_var(var, ast.gtype, "self->f_#{ast.obj.name}")
				elsif ast.obj.is_a?(AST::Function)
					assign_var(var, ast.gtype, nil)
					assign_func(var, nil, ref(ast.obj, ast.gen.last.merge(@map)))
				else
					puts "ref for #{ast.obj.name}, #{ast.gen.first.text} ||| #{ast.gen.last}      map:#{@map}\n#{ast.source.format}"
					assign_var(var, ast.gtype, ref(ast.obj, ast.gen.last.merge(@map)))
				end
			when AST::Return
				result = new_var
				convert(ast.value, result)
				o "*result = #{result.ref};"
				o "return;"
				assign_var(var, [AST::Unit.ctype.type, true], nil)
			when AST::BinOp
				"(#{gen_body.(ast.lhs)} #{ast.op} #{gen_body.(ast.rhs)})"
			when AST::If
				result = "if(#{gen_body(ast.condition)})\n#{ind.()}{\n" + gen_body.(ast.group) + "\n#{ind.()}}"
				result << "\nelse\n#{ind.()}{\n" + gen_body.(ast.else_node) + "\n#{ind.()}}" if ast.else_node
			when AST::Call
				obj = new_var
				convert(ast.obj, obj)
				args = ast.args.map do |a|
					arg = new_var
					convert(a, arg)
					arg.ref
				end
				assign_var(var, ast.gtype, nil)
				o "#{obj.ref}.func(#{([obj.ref + ".data", "&" + var.ref] + args).join(", ")});"
			else
				raise "(unknown #{ast.class.inspect})"
		end
	end
end
