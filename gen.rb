def c_type(ast)
	return ast.to_s if ast.kind_of? Symbol
	case ast.first
		when :ptr
			c_type(ast.last) + "*"
	end
end

def arrayify(ast)
	if ast.first.kind_of? Array
		ast
	else
		[ast]
	end
end

def gen_body(ast)
	apply = proc do |list| arrayify(list).map { |ast| gen_body(ast) } end
	
	case ast.first
		when :string
			ast.last
		when :call
			"#{ast[1].to_s}(#{apply.(ast[2]).join(", ")})"
	end
end

def global_pass(ast, prototype)	
	apply = proc do |list| arrayify(list).map { |ast| global_pass(ast, prototype) } end
	
	case ast.first
		when :global_scope
			apply.(ast.last).join("\n")
		when :func
			opt = ast.last
			return '' if !opt[:group] && !prototype
			
			output = "#{c_type opt[:result]} #{opt[:name]}("
			
			opt[:params].map do |param|
				output << c_type(param.last) << " " << param.first.to_s
			end.join(", ")
			
			output << ")"
			
			if prototype
				output << ";"
			else
				output << "\n{\n"
				output << gen_body(opt[:group])
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