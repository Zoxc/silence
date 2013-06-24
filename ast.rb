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
		
		def outer
			self
		end
		
		def format(lshift = 0)
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
			linestr = "line #{line}: "
			padstr = " " * (untab(source[0...pad]).size + linestr.size)
			count = untab(@input[@range]).size
			rep = count == 1 ? "^" : "~"
			lshift = " " * lshift
			"#{lshift}#{linestr}#{untab(source)}\n#{lshift}#{padstr}#{rep * count}"
		end
	end
	
	class NestedSource
		def initialize(inner, outer)
			@inner = inner
			@outer = outer
		end
		
		def outer
			@outer
		end
		
		def format(lshift = 0)
			"#{@outer.format(lshift)}\n#{@inner.format(lshift + 4)}"
		end
	end
	
	class BuiltinSource
		def initialize(src)
			@src = src
		end
		
		def format(lshift = 0)
			@src.map do |src|
				"#{" " * lshift}<builtin> #{src}"
			end.join
		end
		
		def outer
			self
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
			raise CompileError, "Unable to declare name '#{name}' (#{__id__})\n#{obj.source.format}\nName was already taked by:\n#{existing.source.format}" if existing
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
		attr_accessor :name, :ctype, :type
	
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
	
	class TypeAlias < Node
		attr_accessor :name, :type, :ctype, :type_params
		
		def initialize(source, name, type)
			super(source)
			@name = name
			@type = type
			@type_params = []
		end
		
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
		end
		
		def visit
			@type = yield @type
		end
	end
	
	class Complex < Node
		attr_accessor :name, :scope, :ctype, :type_params, :ctx, :c_prefix
		
		def parent
			ast = @scope.parent.owner
			ast.is_a?(Program) ? nil : ast
		end
		
		def initialize(source, name, scope, type_params, ctx)
			super(source)
			@name = name
			@scope = scope
			@type_params = type_params
			@ctx = ctx
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
	end
	
	class TypeClassInstance < Complex
		attr_accessor :typeclass, :args
		
		def initialize(source, typeclass, args, scope, type_params, ctx)
			super(source, nil, scope, type_params, ctx)
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
	end
	
	class Struct < Complex
		attr_accessor :level, :actions
		
		def initialize(*args)
			super
			@level = :copyable
			@actions = {}
		end
	end
	
	class LocalScope < Scope
	end
	
	class Action < Node
		attr_accessor :scope, :ctype, :action_type
		
		def initialize(source, scope, action_type)
			super(source)
			@scope = scope
			@action_type = action_type
		end
		
		def type_params
			[]
		end
		
		def scoped_name
			name
		end
		
		def props
			{shared: false}
		end
		
		def name
			"action_#{action_type}_#{__id__}"
		end
		
		def declare_pass(scope)
			@scope.owner = self
			@scope.parent = scope
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@scope = yield @scope if @scope
		end
	end
	
	class Function < Node
		attr_accessor :name, :params, :result, :scope, :type, :ctype, :type_params, :props, :type_param_count
		
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
		attr_accessor :name, :value, :var
		
		def initialize(source, right_source, name, type, value, props)
			@source = source
			@name = name
			@props = props
			@var = Variable.new(@source, @name, nil, type, @props)
			@value = BinOp.new(right_source, Ref.new(@source, @var), '=', value) if value
		end
		
		def declare_pass(scope)
			@var.declared = scope.declare(@name, @var)
		end
		
		def visit
			@var = yield @var if @var
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
		attr_accessor :obj, :args, :func, :gen
		
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
	
	class TypeOf < ExpressionNode
		attr_accessor :source, :value
		
		def initialize(source, value)
			@source = source
			@value = value
		end
		
		def visit
			@value = yield @value
		end
	end
end

class Core
	Program = AST::Program.new(AST::Scope.new([]))
	Nodes = Program.scope.nodes
	
	class << self
		def complex(name, args = [], klass = AST::Struct, scope = [], ctx = [])
			r = klass.new(src(2), name, AST::GlobalScope.new(scope), args, ctx)
			Nodes << r
			r
		end
		
		def src(lvl = 1)
			AST::BuiltinSource.new([caller(lvl).first])
		end
		
		def ref(node)
			AST::Ref.new(src(2), node)
		end
		
		def ptr(node)
			AST::UnaryOp.new(src(2), '*', node)
		end
		
		def param(name, tp = nil)
			AST::TypeParam.new(src(2), name, tp)
		end
		
		def func(fname, args, result)
			r = AST::Function.new
			r.source = src(2)
			r.name = fname
			r.params = args.each.map { |name, type| AST::Function::Param.new(src(2), r, name, type) }
			r.type_params = []
			r.result = result
			r.scope = AST::LocalScope.new([])
			r.props = {}
			r
		end
	end
	
	# Typeclasses which allows you to increase required levels in type parameters
	
	class Sizeable < Core
		T = param :T
		Node = complex(:Sizeable, [T], AST::TypeClass)
		
		t = param :T
		Instance = AST::TypeClassInstance.new(src, ref(Sizeable::Node), [ref(t)], AST::GlobalScope.new([]), [t], [])
		Nodes << Instance
	end
	
	class Copyable < Core
		T = param :T
		Node = complex(:Copyable, [T], AST::TypeClass)
		
		t = param :T
		Instance = AST::TypeClassInstance.new(src, ref(Copyable::Node), [ref(t)], AST::GlobalScope.new([]), [t], [])
		Nodes << Instance
	end
	
	
	Unit = complex :Unit
	
	class Tuple < Core
		T = param(:T, ref(Copyable::Node))
		Node = complex(:Tuple, [T], AST::TypeClass)
	end
	
	class Cell < Core
		Val = param(:Val, ref(Copyable::Node))
		#Val = param(:Val)
		Next = param(:Next, ref(Tuple::Node))
		Node = complex(:Cell, [Val, Next])
	end
	
	proc do
		Nodes << AST::TypeClassInstance.new(src, ref(Tuple::Node), [ref(Unit)], AST::GlobalScope.new([]), [], [])
	end.()

	proc do
		val = param :Val
		_next = param :Next
		Nodes << AST::TypeClassInstance.new(src, ref(Tuple::Node), [AST::Index.new(src, ref(Cell::Node), [ref(val), ref(_next)])], AST::GlobalScope.new([]), [val, _next], [])
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
	UInt = complex :uint
	Bool = complex :bool
	String = complex :string
	Char = complex :char
	
	proc do
		ForceCastIn = param :In
		ForceCastOut = param :Out
		ForceCast = func(:force_cast, {in: ref(ForceCastIn)}, ref(ForceCastOut))
		ForceCast.type_params << ForceCastOut << ForceCastIn
		Nodes << ForceCast
	end.()

	class Constructor < Core
		Constructed = AST::TypeFunction.new(src, :Constructed, nil)
		Args = AST::TypeFunction.new(src, :Args, ref(Tuple::Node))
		
		Construct = func(:construct, {obj: ptr(ref(Constructed)), args: ref(Args)}, ref(Unit))
		Construct.props[:shared] = true
		
		T = param :T
		Node = complex(:Constructor, [T], AST::TypeClass, [Constructed, Args, Construct])
	end

	class Callable < Core
		Args = AST::TypeFunction.new(src, :Args, ref(Tuple::Node))
		Result = AST::TypeFunction.new(src, :Result, ref(Sizeable::Node))
		
		Apply = func(:apply, {args: ref(Args)}, ref(Result))
		
		T = param :T
		Node = complex(:Callable, [T], AST::TypeClass, [Args, Result, Apply])
	end

	proc do
		args = param :Args
		result = param :Result
		
		apply = func(:apply, {args: ref(args)}, ref(result))
		
		CallableFuncArgs = args
		CallableFuncApply = apply
		
		Nodes << AST::TypeClassInstance.new(src, ref(Callable::Node), [AST::BinOp.new(src, ref(args), '->', ref(result))], AST::GlobalScope.new([apply]), [args, result], [])
	end.()

	class IntLiteral < Core
		T = param(:T, ref(Sizeable::Node))
		
		Create = func(:create, {input: ref(Int)}, ref(T))
		
		Node = complex(:IntLiteral, [T], AST::TypeClass, [Create])
	end
	
	num_lit = proc do |type|
		create = func(:create, {input: ref(Int)}, ref(type))
		Nodes << AST::TypeClassInstance.new(src, ref(IntLiteral::Node), [ref(type)], AST::GlobalScope.new([create]), [], [])
		create
	end
	
	CharLiteralCreate = num_lit.(Char)
	IntLiteralCreate = num_lit.(Int)
	UIntLiteralCreate = num_lit.(UInt)
	
	class StringLiteral < Core
		T = param(:T, ref(Sizeable::Node))
		
		Create = func(:create, {data: ptr(ref(Char)), length: ref(UInt)}, ref(T))
		
		Node = complex(:StringLiteral, [T], AST::TypeClass, [Create])
	end
	
	class Joinable < Core
		T = param :T
		
		Join = func(:join, {lhs: ref(T), rhs: ref(T)}, ref(T))
		
		Node = complex(:Joinable, [T], AST::TypeClass, [Join])
	end
	
	OpMap = {'~' => {ref: Joinable::Node, param: Joinable::T, func: Joinable::Join}}

	
	Program.run_pass(:declare_pass, false)
	Program.run_pass(:ref_pass)
end
