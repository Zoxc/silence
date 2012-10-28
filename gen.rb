def c_type(ast)
	case ast
		when AST::NamedTypeNode
			ast.name.to_s
		when AST::PtrTypeNode
			c_type(ast.target) + "*"
		when AST::NullPtrTypeNode
			c_type(ast.target) + "^"
	end
end

def ident_str(str)
	str.lines.map { |l| "    " + l }.join
end

def gen_body(ast)
	apply = proc do |list| list.map { |ast| gen_body(ast) } end
	
	case ast
		when AST::Scope
			ident_str apply.(ast.nodes).map { |e| e + ";" }.join("\n")
		when AST::IntegerLiteral
			ast.value.to_s
		when AST::VariableRef
			ast.name.to_s
		when AST::Return
			"return #{gen_body(ast.value)}"
		when AST::BinOp
			"(#{gen_body(ast.lhs)} #{ast.op} #{gen_body(ast.rhs)})"
		when AST::StringLiteral
			ast.value
		when AST::If
			result = "if(#{gen_body(ast.condition)})\n{\n" + gen_body(ast.group) + "\n}"
			result << "\nelse\n{\n" + gen_body(ast.else_node) + "\n}" if ast.else_node
		when AST::Call
			"#{ast.name}(#{apply.(ast.args).join(", ")})"
		else
			raise "(unknown #{ast.class.inspect} - #{ast.inspect})"
	end
end

def global_pass(ast, prototype)	
	apply = proc do |nodes| nodes.map { |ast| global_pass(ast, prototype) } end
	
	case ast
		when AST::GlobalScope
			apply.(ast.nodes).join("\n")
		when AST::Function
			return '' if !ast.group && !prototype
			
			output = "#{c_type ast.result} #{ast.name}("
			
			output << ast.params.map { |param| "#{c_type(param.type)} #{param.name}" }.join(", ") << ")"
			
			if prototype
				output << ";"
			else
				output << "\n{\n"
				output << gen_body(ast.group)
				output << "\n}"
			end
			
			output
	end
end

def codegen(ast)
	output = global_pass(ast, true)
	output << "\n"
	output << global_pass(ast, false)
	output
end