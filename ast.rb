require_relative 'types'

module AST
	class Source
		attr_accessor :input, :range
		
		def initialize(input, range)
			@input = input
			@range = range
		end
		
		def extend(pos)
			@range = (@range.first)...([pos, @range.last].max)
		end
		
		def untab(str)
			str.gsub("\t", " " * 8)
		end
		
		def format
			lines = @input.lines.to_a
			i = 0
			line = 1
			while i < @range.min - lines.first.chomp.size
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
		attr_accessor :source, :declared
		
		def run_pass(name, replace = false, args = nil, apply = :apply_pass)
			result = if respond_to? name
					send name, args
				else
					self
				end
			
			args = send apply, args if apply
			
			visit do |node|
				next unless node
				
				node.run_pass(name, replace, args)
			end
			
			if replace
				result
			else
				self
			end
		end
		
		def dup_ast
			result = dup
			
			visit do |node|
				next unless node
				node.dup
			end
			
			result
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
		attr_accessor :name, :type, :declared, :ctype
		
		def initialize(source, name, declared, type)
			super(source)
			@name = name
			@declared = declared
			@type = type
		end
		
		def visit
			@type = yield @type
		end
	end
	
	class Index < Node
		attr_accessor :obj, :args
		
		def initialize(source, obj, args)
			super(source)
			@obj = obj
			@args = args
		end
		
		def visit
			@obj = yield @obj
			@args.map! { |n| yield n }
		end
	end

	class FunctionType < Node
		attr_accessor :arg, :result
		
		def initialize(source, arg, result)
			super(source)
			@arg = arg
			@result = result
		end
		
		def visit
			@arg = yield @arg
			@result = yield @result
		end
	end

	class TypeCheck < Node
		attr_accessor :node, :type
		
		def initialize(source, node, type)
			super(source)
			@node = node
			@type = type
		end
		
		def visit
			@node = yield @node
			@type = yield @type
		end
	end

	class Grouped < Node
		attr_accessor :node
		
		def initialize(source, node)
			super(source)
			@node = node
		end
		
		def visit
			@node = yield @node
		end
	end

	class Scope < Node
		attr_accessor :nodes, :names, :parent, :owner
		
		def initialize(nodes)
			@nodes = nodes
			@names = {}
		end
		
		def declare(name, obj)
			#puts "|declaring #{name} in #{__id__} \n#{obj.source.format if obj.source}|"
			@names[name] = obj
			self
		end
		
		def inside?(scope)
			return true if scope == self
			return @parent.inside?(scope) if parent
			return false
		end
		
		def require(source, name, err_msg = proc { "Unknown identifier '#{name}'" })
			r = require_with_scope(source, name, err_msg)
			r ? r.first : nil
		end
		
		def require_with_scope(source, name, err_msg)
			#puts "|looking up identifier #{name} in #{__id__} \n#{source.format}|#{@names.keys.inspect}"
			result = @names[name]
			if result
				return [result, self]
			end
			return @parent.require_with_scope(source, name, err_msg) if @parent
			raise CompileError, "#{err_msg.()}\n#{source.format}"
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
			@scope.owner = self
		end
		
		def visit
			@scope = yield scope
		end
	end

	class Tuple < Node
		attr_accessor :nodes
	
		def initialize(source, nodes)
			super(source)
			@nodes = nodes
		end
	end
	
	class TypeFunction < Node
		attr_accessor :name, :ctype
	
		def initialize(source, name)
			super(source)
			@name = name
		end
		
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
		end
	end

	class Complex < Node
		attr_accessor :name, :scope, :ctype, :params
		
		class Param < Node
			attr_accessor :name, :type, :owner, :ctype
			
			def initialize(source, name, type)
				super(source)
				@name = name
				@type = type
			end
			
			def visit
				@type = yield @type if @type
			end
		end
		
		def parent
			ast = @scope.parent.owner
			ast.is_a?(Program) ? nil : ast
		end
		
		def initialize(source, name, scope, params)
			super(source)
			@name = name
			@scope = scope
			@params = params
		end
		
		def declare_pass(scope)
			(@declared = scope.declare(@name, self)) if @name
			@scope.parent = scope
			@scope.owner = self
			
			@params.each do |param|
				param.owner = self
				param.declared = @scope.declare(param.name, param)
			end
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@params.map! { |n| yield n }
			@scope = yield scope
		end
	end
	
	class TypeClass < Complex
		attr_reader :instances
		
		def initialize(*args)
			super
			@instances = []
		end
		
		def type_class?
			true
		end
	end
	
	class TypeClassInstance < Complex
		attr_accessor :typeclass, :args
		
		def initialize(source, typeclass, args, scope, params)
			super(source, nil, scope, params)
			@typeclass = typeclass
			@args = args
		end
		
		def ref_pass(scope)
			@typeclass.obj.instances << self
		end
		
		def name
			"##{@typeclass.obj.name}"
		end
		
		def visit
			super
			@args.map! { |n| yield n }
			@typeclass = yield @typeclass
		end
		
		def type_class?
			false
		end
	end
	
	class Struct < Complex
		def type_class?
			false
		end
	end
	
	class LocalScope < Scope
	end
	
	class Function < Node
		attr_accessor :name, :params, :result, :attributes, :scope, :type, :ctype, :parent_scope, :instances
		
		class Param < Node
			attr_accessor :name, :type, :var
			
			def initialize(source, func, name, type)
				super(source)
				@func = func
				@name = name
				@type = type
			end
			
			def declare_pass(scope)
				if scope
					@var = Variable.new(@source, @name, scope, @type)
					@declared = scope.declare(@name, @var)
				end
			end
			
			def visit
				@var = yield @var if @var
				@type = yield @type if @type
			end
			if nil
				def sema(scope)
					if scope
						Ref.new(@source, @var)
					else
						self
					end
				end
			end
		end
	
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
			
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
			@result = yield @result
			@scope = yield @scope if @scope
		end
	end
	
	class UnaryOp < ExpressionNode
		attr_accessor :op, :node
		
		def initialize(source, op, node)
			super(source)
			@op = op
			@node = node
		end
		
		def visit
			@node = yield @node
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
			@var = Variable.new(@source, @name, scope, @type)
			@declared = scope.declare(@name, @var)
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
			@var = yield @var if @var
			@type = yield @type if @type
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
	
	class NameRef < ExpressionNode
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

	class Field < ExpressionNode
		attr_accessor :obj, :name
		
		def initialize(source, obj, name)
			@source = source
			@obj = obj
			@name = name
		end
		
		def visit
			@obj = yield @obj
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
	
	Src = Object.new
	class << Src
		def format
			"  <builtin>\n\n"
		end
	end
	
	Builtin = Program.new(Scope.new([]))
	BuiltinNodes = Builtin.scope.nodes
	
	def self.complex_type(name, args = [], klass = Struct, scope = [])
		r = klass.new(Src, name, GlobalScope.new(scope), args)
		BuiltinNodes << r
		r
	end
	
	Unit = complex_type(:Unit)
	
	module Cell
		Val = Complex::Param.new(Src, :Val, nil)
		Next = Complex::Param.new(Src, :Next, nil)
		Node = AST.complex_type(:Cell, [Val, Next])
	end
	
	Int = complex_type(:int)
	Bool = complex_type(:bool)
	String = complex_type(:string)
	Char = complex_type(:char)
	
	module Callable
		Args = TypeFunction.new(Src, :Args)
		Result = TypeFunction.new(Src, :Result)
		T = Complex::Param.new(Src, :T, nil)
		Node = AST.complex_type(:Callable, [T], TypeClass, [Args, Result])
	end

	proc do
		args = Complex::Param.new(Src, :Args, nil)
		result = Complex::Param.new(Src, :Result, nil)
		BuiltinNodes << TypeClassInstance.new(Src, Ref.new(Src, Callable::Node), [FunctionType.new(Src, Ref.new(Src, args), Ref.new(Src, result))], GlobalScope.new([]), [args, result])
	end.()

	Builtin.run_pass(:declare_pass, false)
	Builtin.run_pass(:ref_pass)
end
