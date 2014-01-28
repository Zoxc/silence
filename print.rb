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
		when AST::Literal
			format.(ast, [[:value]])
		when AST::VariableDecl
			format.(ast, [[:name], [:var, :single], [:value, :single]])
		when AST::Return
			format.(ast, [[:value, :single]])
		when AST::Ref
			format.(ast, [[:name, :value, ast.obj.name], [:id, :value, ast.obj.__id__]])
		when AST::Variable
			format.(ast, [[:name], [:props]])
		when AST::NameRef
			format.(ast, [[:name]])
		when AST::Apply
			format.(ast, [[:obj, :single], [:arg, :single]])
		when AST::Call
			format.(ast, [[:obj, :single], [:args, ast.args]])
		when AST::Field
			format.(ast, [[:obj, :single], [:name]])
		when AST::BinOp
			format.(ast, [[:lhs, :single], [:op], [:rhs, :single]])
		when AST::UnaryOp
			format.(ast, [[:node, :single], [:op]])
		when AST::Index
			format.(ast, [[:obj, :single], [:args, ast.args]])
		when AST::TypeTuple, AST::ValueTuple, AST::Array
			format.(ast, [[:nodes, ast.nodes]])
		when AST::Grouped
			format.(ast, [[:node, :single]])
		when AST::If
			format.(ast, [[:condition, :single], [:group, :single], [:else_node, :single]])
		when AST::Program
			format.(ast, [[:scope, :single]])
		when AST::Scope
			format.(ast, [[:id, :value, ast.__id__], [:nodes, ast.nodes]])
		when AST::TypeClassInstance
			format.(ast, [[:kind_params, :single], [:typeclass, :single], [:args, ast.args], [:scope, :single]])
		when AST::KindParams
			format.(ast, [[:params, ast.params], [:ctx, ast.ctx]])
		when AST::MatchBinding
			format.(ast, [[:name]])
		when AST::MatchWhen
			format.(ast, [[:type, :single], [:group, :single]])
		when AST::MatchAs
			format.(ast, [[:expr, :single], [:binding, :single], [:else_group, :single], [:whens, ast.whens]])
		when AST::Complex
			format.(ast, [[:name], [:id, :value, ast.__id__], [:scope, :single], [:kind_params, :single]])
		when AST::TypeParam
			format.(ast, [[:name], [:id, :value, ast.__id__], [:kind_params, :single], [:type, :single]])
		when AST::TypeFunction
			format.(ast, [[:name], [:id, :value, ast.__id__]])
		when AST::Function
			format.(ast, [[:name], [:props], [:id, :value, ast.__id__], [:result, :single], [:params, ast.params], [:scope, :single], [:kind_params, :single]])
		when AST::Function::Param
			format.(ast, [[:name], [:id, :value, ast.__id__], [:type, :single]])
		when AST::TypeOf
			format.(ast, [[:value, :single]])
		when AST::TypeAlias
			format.(ast, [[:name], [:id, :value, ast.__id__], [:type, :single]])
		else
			"(unknown #{ast.class.inspect})"
	end
end
