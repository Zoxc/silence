def print_ast(ast)	
	apply = proc do |nodes| nodes.map { |ast| print_ast(ast) } end
	ident = proc do |str| str.lines.map { |l| "    " + l }.join end
	nest = proc do |ast, props| ast.class.name + "\n" + ident.(props) end
	format = proc do |ast, props|
		result = ast.class.name + "\n"
		result << ident.(props.map do |prop|
			r = "#{prop.first}: "
			nest = proc do |str| r << "\n" << ident.(str) end
			if prop.size == 1
				r << ast.send(prop.first).inspect
			else
				case prop[1]
					when Array
						nest.(apply.(prop[1]).join("\n"))
					when :single
						nest.(print_ast(ast.send(prop.first)))
					when :value
						r << prop[2].inspect
					when :inline
						r << print_ast(ast.send(prop.first))
				end
			end
			r
		end.join("\n"))
	end
	
	case ast
		when nil
			"(nil)"
		when AST::NamedTypeNode
			ast.name.to_s
		when AST::PtrTypeNode
			"*" + print_ast(ast.target)
		when AST::NullPtrTypeNode
			"^" + print_ast(ast.target)
		
		when AST::Literal
			format.(ast, [[:value]])
		when AST::VariableDecl
			format.(ast, [[:name], [:type, :inline], [:value, :single]])
		when AST::Return
			format.(ast, [[:value, :single]])
		when AST::Ref
			format.(ast, [[:name, :value, ast.obj.name], [:id, :value, ast.obj.__id__]])
		when AST::Variable
			format.(ast, [[:name]])
		when AST::VariableRef
			format.(ast, [[:name]])
		when AST::Call
			format.(ast, [[:obj, :single], [:args, ast.args]])
		when AST::BinOp
			format.(ast, [[:lhs, :single], [:op], [:rhs, :single]])
		when AST::If
			format.(ast, [[:condition, :single], [:group, :single], [:else_node, :single]])
		
		when AST::Program
			format.(ast, [[:scope, :single]])
		when AST::GlobalScope, AST::Scope
			format.(ast, [[:nodes, ast.nodes]])
		when AST::Function
			format.(ast, [[:name], [:id, :value, ast.__id__], [:attributes], [:result, :inline], [:params, ast.params], [:scope, :single]])
		when AST::Function::Parameter
			format.(ast, [[:name], [:type, :inline]])
		else
			"(unknown #{ast.class.inspect} - #{ast.inspect})"
	end
end
