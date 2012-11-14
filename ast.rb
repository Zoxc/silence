require 'treetop'

module AST
	class Source
		def initialize(input, range)
			@input = input
			@range = range
		end
		
		def untab(str)
			str.gsub("\t", " " * 8)
		end
		
		def format
			lines = @input.lines.to_a
			i = 0
			line = 1
			while i < @range.min - lines.first.size
				i += lines.first.size
				line += 1
				lines.shift
			end
			count = [@range.max - @range.min, lines.first.chomp.size].max
			pad = @range.min - i
			source = @input[i, count]
			linestr = "  line #{line}: "
			padstr = " " * (untab(source[0...pad]).size + linestr.size)
			count = untab(@input[@range]).size
			rep = count == 1 ? "^" : "~"
			"#{linestr}#{untab(source)}\n#{padstr}#{rep * count}"
		end
	end
	
	class Node
		attr_accessor :source
		
		def run_pass(name, replace = false, scope = nil)
			result = if respond_to? name
					send name, scope
				else
					self
				end
			
			scope = apply_pass(scope)
			
			visit do |node|
				next unless node
				
				node.run_pass(name, replace, scope)
			end
			
			if replace
				result
			else
				self
			end
		end
		
		def apply_pass(scope)
			scope
		end
		
		def visit
		end
		
		def initialize(source = nil)
			@source = source
		end
	end
	
	class ExpressionNode < Node
	end
	
	class Variable < Node
		attr_accessor :name, :type
		
		def initialize(source, name, type)
			super(source)
			@name = name
			@type = type
		end
	end
	
	class NamedTypeNode < Node
		attr_accessor :name
		
		def initialize(source, name)
			super(source)
			@name = name
		end
		
		def to_s
			@name
		end
	end

	class NullPtrTypeNode < Node
		attr_accessor :target
		
		def initialize(source, target)
			super(source)
			@target = target
		end
		
		def to_s
			"^#{@target.inspect}"
		end
	end

	class PtrTypeNode < Node
		attr_accessor :target
		
		def initialize(source, target)
			super(source)
			@target = target
		end
		
		def to_s
			"*#{@target.inspect}"
		end
	end

	class Scope < Node
		attr_accessor :nodes, :names, :parent
		
		def initialize(nodes)
			@nodes = nodes
			@names = {}
		end
		
		def declare(name, obj)
			#puts "|declaring #{name} in #{__id__} \n#{obj.source.format if obj.source}|"
			@names[name] = obj
		end
		
		def require(source, name , type = nil)
			#puts "|looking up identifier #{name} in #{__id__} \n#{source.format}|#{@names.keys.inspect}"
			result = @names[name]
			if result
				if type && !result.kind_of?(type)
					raise CompileError, "Found type '#{result.class}', but expected type '#{type}' for '#{name}'\n#{source.format}"
				end
				return result
			end
			return @parent.require(source, name, type) if @parent
			raise CompileError, "Unknown identifier '#{name}'\n#{source.format}"
		end
		
		def visit
			@nodes.map! { |n| yield n }
		end
	end
	
	class GlobalScope < Scope
	end

	class Program < Node
		attr_accessor :scope
	
		def initialize(scope)
			@scope = scope
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def declare_pass(scope)
			@scope.parent = scope
		end
		
		def visit
			@scope = yield scope
		end
	end

	class LocalScope < Scope
	end
	
	class Function < Node
		attr_accessor :name, :params, :result, :attributes, :scope, :type, :itype, :parent_scope, :constraints
		
		class Parameter < Node
			attr_accessor :name, :type, :var
			
			def initialize(source, name, type)
				super(source)
				@name = name
				@type = type
			end
			
			def declare_pass(scope)
				if scope
					@var = Variable.new(@source, @name, @type)
					scope.declare(@name, @var)
				end
			end
		end
	
		def declare_pass(scope)
			scope.declare(@name, self)
			
			if @scope
				@scope.parent = scope
			else
				@parent_scope = scope
			end
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@params.map! { |n| yield n }
			@scope = yield scope if @scope
		end
	end
	
	class BinOp < ExpressionNode
		attr_accessor :lhs, :op, :rhs
		
		def initialize(source, lhs, op, rhs)
			super(source)
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
		
		def initialize(source, right_source, name, type, value)
			@source = source
			@right_source = right_source
			@name = name
			@value = value
			@type = type
		end
		
		def declare_pass(scope)
			@var = Variable.new(@source, @name, @type)
			scope.declare(@name, @var)
		end
		
		def sema(scope)
			ref = Ref.new(@source, @var)
			
			if @value
				BinOp.new(@right_source, ref, :'=', @value)
			else
				ref
			end
		end
		
		def visit
			@value = yield @value if @value
		end
	end
	
	class Ref < ExpressionNode
		attr_accessor :obj
		
		def initialize(source, obj)
			super(source)
			@obj = obj
		end
	end
	
	class VariableRef < ExpressionNode
		attr_accessor :name
		
		def initialize(source, name)
			super(source)
			@name = name
		end
		
		def sema(scope)
			Ref.new @source, scope.require(@source, @name)
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

	class Literal < ExpressionNode
		attr_accessor :type, :value
		
		def initialize(source, type, value)
			super(source)
			@type = type
			@value = value
		end
	end

	class Call < ExpressionNode
		attr_accessor :obj, :args, :func
		
		def initialize(source, obj, args)
			@source = source
			@obj = obj
			@args = args
		end
		
		def visit
			@args.map! { |n| yield n }
			@obj = yield @obj
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

	BuiltinScope = AST::Scope.new([])
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
				result = AST::BinOp.new(re[:op].source, result, re[:op].text_value.to_sym, re[:rhs])
			end until rl_ast.empty?
			
			result
		end
	end
	
	def rhe(op, rhs)
		{op: op, rhs: single_ast(rhs)}
	end
	
	def source
		AST::Source.new(input, interval)
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
