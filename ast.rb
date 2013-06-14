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
		attr_accessor :source, :declared, :gtype
		
		def scoped_name
			owner = declared.owner
			"#{"#{owner.scoped_name}." unless owner.is_a?(Program)}#{name}"
		end
		
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
		attr_accessor :name, :type, :ctype, :props
		
		def initialize(source, name, declared, type, props)
			super(source)
			@name = name
			@declared = declared
			@type = type
			@props = props
		end
		
		def type_params
			[]
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
			existing = @names[name]
			raise CompileError, "Unable to declare name '#{name}'\n#{obj.source.format}\nName was already taked by:\n#{existing.source.format}" if existing
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

	class Array < Node
		attr_accessor :nodes
	
		def initialize(source, nodes)
			super(source)
			@nodes = nodes
		end
		
		def visit
			@nodes.map! { |n| yield n }
		end
	end
	
	class Tuple < Node
		attr_accessor :nodes
	
		def initialize(source, nodes)
			super(source)
			@nodes = nodes
		end
		
		def visit
			@nodes.map! { |n| yield n }
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

	class TypeParam < Node
		attr_accessor :name, :type, :ctype
		
		def initialize(source, name, type)
			super(source)
			@name = name
			@type = type
		end
		
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
		end
		
		def visit
			@type = yield @type if @type
		end
	end
	
	class Complex < Node
		attr_accessor :name, :scope, :ctype, :type_params, :c_prefix
		
		def parent
			ast = @scope.parent.owner
			ast.is_a?(Program) ? nil : ast
		end
		
		def initialize(source, name, scope, type_params)
			super(source)
			@name = name
			@scope = scope
			@type_params = type_params
			@c_prefix = true
		end
		
		def declare_pass(scope)
			(@declared = scope.declare(@name, self)) if @name
			@scope.parent = scope
			@scope.owner = self
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@type_params.map! { |n| yield n }
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
		
		def initialize(source, typeclass, args, scope, type_params)
			super(source, nil, scope, type_params)
			@typeclass = typeclass
			@args = args
		end
		
		def ref_pass(scope)
			@typeclass.obj.instances << self
		end
		
		def scoped_name
			name
		end
		
		def name
			"##{@typeclass.obj.name}_#{__id__}"
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
		attr_accessor :name, :params, :result, :attributes, :scope, :type, :ctype, :instances, :type_params, :props, :type_param_count
		
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
					@var = Variable.new(@source, @name, scope, @type, @props)
					@declared = scope.declare(@name, @var)
				end
			end
			
			def visit
				@var = yield @var if @var
				@type = yield @type if @type
			end
		end
	
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
			@scope.owner = self
			@scope.parent = scope if @scope
			@type_param_count = @type_params.size
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@params.map! { |n| yield n }
			@type_params.map! { |n| yield n }
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
		attr_accessor :name, :value, :type, :declared
		
		def initialize(source, right_source, name, type, value, props)
			@source = source
			@right_source = right_source
			@name = name
			@value = value
			@type = type
			@props = props
		end
		
		def declare_pass(scope)
			@var = Variable.new(@source, @name, scope, @type, @props)
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
		attr_accessor :obj, :gen
		
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
		attr_accessor :obj, :name, :gen
		
		def initialize(source, obj, name)
			@source = source
			@obj = obj
			@name = name
		end
		
		def visit
			@obj = yield @obj
		end
	end
	
	class Apply < ExpressionNode
		attr_accessor :obj, :arg, :func
		
		def initialize(source, obj, arg)
			@source = source
			@obj = obj
			@arg = arg
		end
		
		def visit
			@obj = yield @obj
			@arg = yield @arg
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
end

class Core
	Src = Object.new
	class << Src
		def format
			"  <builtin>\n\n"
		end
	end
	
	Program = AST::Program.new(AST::Scope.new([]))
	Nodes = Program.scope.nodes
	
	class << self
		def complex(name, args = [], klass = AST::Struct, scope = [])
			r = klass.new(Src, name, AST::GlobalScope.new(scope), args)
			Nodes << r
			r
		end
		
		def ref(node)
			AST::Ref.new(Src, node)
		end
		
		def param(name)
			AST::TypeParam.new(Src, name, nil)
		end
	end
	
	Unit = complex :Unit
	
	class Cell < Core
		Val = param :Val
		Next = param :Next
		Node = complex(:Cell, [Val, Next])
	end
	
	class Tuple < Core
		T = param :T
		Node = complex(:Tuple, [T], AST::TypeClass)
	end
	
	proc do
		Nodes << AST::TypeClassInstance.new(Src, ref(Tuple::Node), [ref(Unit)], AST::GlobalScope.new([]), [])
	end.()

	proc do
		val = param :Val
		_next = param :Next
		Nodes << AST::TypeClassInstance.new(Src, ref(Tuple::Node), [AST::Index.new(Src, ref(Cell::Node), [ref(val), ref(_next)])], AST::GlobalScope.new([]), [val, _next])
	end.()

	class Func < Core
		Args = param :Args
		Result = param :Result
		Node = complex(:Func, [Args, Result])
	end
	
	class Ptr < Core
		Type = param :Type
		Node = complex(:Ptr, [Type])
	end
	
	Int = complex :int
	Bool = complex :bool
	String = complex :string
	Char = complex :char
	
	class Callable < Core
		Args = AST::TypeFunction.new(Src, :Args) # Constrain this to Tuple
		Result = AST::TypeFunction.new(Src, :Result)
		
		Apply = AST::Function.new
		Apply.source = Src
		Apply.name = :apply
		Apply.params = [AST::Function::Param.new(Src, Apply, :args, ref(Args))]
		Apply.type_params = []
		Apply.result = ref(Result)
		Apply.attributes = []
		Apply.scope = AST::LocalScope.new([])
		Apply.props = []
		
		T = param :T
		Node = complex(:Callable, [T], AST::TypeClass, [Args, Result, Apply])
	end

	proc do
		args = param :Args
		result = param :Result
		
		apply = AST::Function.new
		apply.source = Src
		apply.name = :apply
		apply.params = [AST::Function::Param.new(Src, apply, :args, ref(args))]
		apply.type_params = []
		apply.result = ref(result)
		apply.attributes = []
		apply.scope = AST::LocalScope.new([])
		apply.props = []
		
		Nodes << AST::TypeClassInstance.new(Src, ref(Callable::Node), [AST::BinOp.new(Src, ref(args), '->', ref(result))], AST::GlobalScope.new([apply]), [args, result])
	end.()

	Program.run_pass(:declare_pass, false)
	Program.run_pass(:ref_pass)
end
