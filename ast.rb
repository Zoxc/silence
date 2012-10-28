require 'treetop'

class NoNode
end

class Treetop::Runtime::SyntaxNode
	def auto_ast(input = self, merge = true)
		return NoNode unless input.elements
		
		result = input.elements.map(&:ast).reject { |v| v == NoNode }
		
		case result.size
			when 0
				merge ? NoNode : []
			when 1
				merge ? result.first : result
			else
				result
		end
	end
	
	def opt_ast(nodes)
		if nodes.elements
			auto_ast(nodes)
		end
	end
	
	def ast
		auto_ast(self, true)
	end
end
