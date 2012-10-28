require 'treetop'

module AST
	class Node
	end
	
	class Function < Node
		attr_accessor :name, :params, :result, :attributes, :group
		
		class Parameter < Node
			attr_accessor :name, :type
			
			def initialize(name, type)
				@name = name
				@type = type
			end
		end
	end
	
	class Scope < Node
		attr_accessor :nodes
		
		def initialize(nodes)
			@nodes = nodes
		end
	end
	
	class BinOp < Node
		attr_accessor :lhs, :op, :rhs
		
		def initialize(lhs, op, rhs)
			@lhs = lhs
			@op = op
			@rhs = rhs
		end
	end
	
	class VariableRef < Node
		attr_accessor :name
		
		def initialize(name)
			@name = name
		end
	end
	
	class If < Node
		attr_accessor :condition, :group, :else_node
		
		def initialize(condition, group, else_node)
			@condition = condition
			@group = group
			@else_node = else_node
		end
	end

	class NamedType < Node
		attr_accessor :name
		
		def initialize(name)
			@name = name
		end
		
		def to_s
			@name
		end
	end

	class NullPtrType < Node
		attr_accessor :target
		
		def initialize(target)
			@target = target
		end
		
		def to_s
			"^#{@target.inspect}"
		end
	end

	class PtrType < Node
		attr_accessor :target
		
		def initialize(target)
			@target = target
		end
		
		def to_s
			"*#{@target.inspect}"
		end
	end

	class GlobalScope < Node
		attr_accessor :nodes
		
		def initialize(nodes)
			@nodes = nodes
		end
	end

	class IntegerLiteral < Node
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
	end

	class StringLiteral < Node
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
	end

	class Call < Node
		attr_accessor :name, :args
		
		def initialize(name, args)
			@name = name
			@args = args
		end
	end
	
	class Return < Node
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
	end
end

class Treetop::Runtime::SyntaxNode
	def auto_ast
		return [] unless elements
		
		elements.map(&:ast).flatten
	end
	
	def merge_rhe(lhs, rl, left = true)
		rl_ast = rl.ast
		
		if rl_ast.empty?
			auto_ast
		else
			result = single_ast(lhs)
			
			begin
				re = left ? rl_ast.pop : rl_ast.shift
				result = AST::BinOp.new(result, re[:op], re[:rhs])
			end until rl_ast.empty?
			
			result
		end
	end
	
	def rhe(op, rhs)
		{op: op.text_value.to_sym, rhs: single_ast(rhs)}
	end
	
	def single_ast(nodes)
		[*nodes.ast].first
	end
	
	alias ast auto_ast
	
	alias inspect_old inspect
	
	def inspect(*args)
		"#{ast.class.inspect.ljust(30)} - " + inspect_old(*args)
	end
end
