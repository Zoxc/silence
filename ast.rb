require 'treetop'

module AST
	class Node
		def run_declare_pass(scope)
			scope = apply_pass(scope)
			
			visit do |node|
				next unless node
				
				node.run_declare_pass(scope)
				node.declare_pass(scope)
				node
			end
		end
		
		def sema_pass(scope)
			scope = apply_pass(scope)
			
			visit do |node|
				next unless node
				
				node.sema_pass(scope)
				node.sema(scope)
			end
		end
		
		def apply_pass(scope)
			scope
		end
		
		def visit
		end
		
		def declare_pass(scope)
			self
		end
		
		def sema(scope)
			self
		end
	end
	
	class ExpressionNode < Node
	end
	
	class Variable < Node
		attr_accessor :name, :type
		
		def initialize(name, type)
			@name = name
			@type = type
		end
	end
	
	class NamedTypeNode < Node
		attr_accessor :name
		
		def initialize(name)
			@name = name
		end
		
		def to_s
			@name
		end
	end

	class NullPtrTypeNode < Node
		attr_accessor :target
		
		def initialize(target)
			@target = target
		end
		
		def to_s
			"^#{@target.inspect}"
		end
	end

	class PtrTypeNode < Node
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
		
		def visit
			@nodes.map! { |n| yield n }
		end
	end

	class Function < Node
		attr_accessor :name, :params, :result, :attributes, :group
		
		class Parameter < Node
			attr_accessor :name, :type
			
			def initialize(name, type)
				@name = name
				@type = type
			end
			
			def declare(scope)
				scope.declare(@name, @type) if scope
			end
		end
		
		def apply_pass(scope)
			@group
		end
		
		def visit
			@params.map! { |n| yield n }
			@group = yield group
		end
	end
	
	class Scope < Node
		attr_accessor :nodes, :variables
		
		def initialize(nodes)
			@nodes = nodes
			@variables = {}
		end
		
		def declare(name, type)
			@variables[name] = Variable.new(name, type)
		end
		
		def visit
			@nodes.map! { |n| yield n }
		end
	end
	
	class BinOp < ExpressionNode
		attr_accessor :lhs, :op, :rhs
		
		def initialize(lhs, op, rhs)
			@lhs = lhs
			@op = op
			@rhs = rhs
		end
		
		def visit
			@lhs = yield @lhs
			@rhs = yield @rhs
		end
	end
	
	class VariableDecl < ExpressionNode
		attr_accessor :name, :value, :type
		
		def initialize(name, type, value)
			@name = name
			@value = value
			@type = type
		end
		
		def declare_pass(scope)
			@var = scope.declare(name, type)
		end
		
		def sema(scope)
			if @value
				BinOp.new(@var, :'=', @value)
			else
				@var
			end
		end
		
		def visit
			@value = yield @value if @value
		end
	end
	
	class VariableRef < ExpressionNode
		attr_accessor :name
		
		def initialize(name)
			@name = name
		end
		
		def sema(scope)
			result = scope.variables[@name]
			raise "Unknown variable #{@name}" unless result
			result
		end
	end
	
	class If < ExpressionNode
		attr_accessor :condition, :group, :else_node
		
		def initialize(condition, group, else_node)
			@condition = condition
			@group = group
			@else_node = else_node
		end
		
		def visit
			@condition = yield @condition
			@group = yield @group
			@else_node = yield @else_node
		end
	end

	class IntegerLiteral < ExpressionNode
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
	end

	class StringLiteral < ExpressionNode
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
	end

	class Call < ExpressionNode
		attr_accessor :name, :args
		
		def initialize(name, args)
			@name = name
			@args = args
		end
		
		def visit
			@args.map! { |n| yield n }
		end
	end
	
	class Return < ExpressionNode
		attr_accessor :value
		
		def initialize(value)
			@value = value
		end
		
		def visit
			@value = yield @value
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
		ast = nodes.ast
		case ast
			when Array
				ast.first
			else
				ast
		end
	end
	
	alias ast auto_ast
	
	alias inspect_old inspect
	
	def inspect(*args)
		"#{ast.class.inspect.ljust(30)} - " + inspect_old(*args)
	end
end