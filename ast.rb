require_relative 'types'

module AST
	Input = Struct.new(:src, :file)

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
			lines = @input.src.lines.to_a
			i = 0
			line = 1
			while i < @range.min - lines.first.chomp.size
				i += lines.first.size
				line += 1
				lines.shift
			end
			count = [@range.max - @range.min, lines.first.chomp.size].max
			pad = @range.min - i
			source = @input.src[i, count]
			linestr = "#{@input.file}:#{line}: "
			padstr = " " * (untab(source[0...pad]).size + linestr.size)
			count = untab(@input.src[@range]).size
			rep = count == 1 ? "^" : "~"
			lshift = " " * lshift
			"#{lshift}#{linestr}#{untab(source)}\n#{lshift}#{padstr}#{rep * count}"
		end
	end
	
	class NestedSource
		def initialize(inner, outer)
			@inner = inner
			@outer = outer
			raise "missing inner" unless inner
			raise "missing outer" unless outer
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
		attr_accessor :source, :declared
		
		def scoped_name
			raise "declared not set on #{name}\n#{source.format}" unless declared
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
		attr_accessor :name, :type, :ctype, :props, :type_params, :decl
		
		def initialize(source, name, declared, type, props, decl = nil)
			super(source)
			@name = name
			@declared = declared
			@type = type
			@props = props
			@type_params = []
			@decl = decl
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
		
		def fscope
			c = self
			while c
				return c if c.is_a?(FuncScope)
				c = c.parent
			end
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
		
		def require(source, name)
			r, scope = require_with_scope(name)
			raise CompileError, "Unknown identifier '#{name}'\n#{source.format}" unless r
			r_fscope = scope.fscope
			if r.is_a?(AST::Variable) && r_fscope
				fscope.req_var(r, r_fscope)
			end
			r
		end
		
		def require_with_scope(name)
			#puts "|looking up identifier #{name} in #{__id__} \n#{source.format}|#{@names.keys.inspect}"
			result = @names[name]
			return [result, self] if result
			return @parent.require_with_scope(name) if @parent
			[nil, nil]
		end
		
		def visit
			@nodes.map! { |n| yield n }
		end
	end
	
	class FuncScope < Scope
		attr_accessor :req_vars

		def initialize(nodes)
			super
			@req_vars = []
		end

		def req_var(var, fscope)
			return if fscope == self

			@req_vars.push(var) unless @req_vars.include?(var)

			fparent = parent.fscope
			fparent.req_var(var, fscope) if fparent
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

	class KindParams < Node
		attr_accessor :params, :ctx
	
		def initialize(source, params, ctx)
			super(source)
			@params = params
			@ctx = ctx
		end
		
		def visit
			@ctx.map! { |n| yield n }
			@params.map! { |n| yield n }
		end
	end
	
	class HigherKinded < Node
		attr_accessor :type_params, :scope, :kind_params
		
		def initialize(source, kind_params)
			super(source)
			@kind_params = kind_params
			@scope = LocalScope.new([]) if !type_params.empty? and !@scope
		end

		def type_params
			@kind_params.params
		end
		
		def declare_pass(scope)
			if @scope
				@scope.parent = scope
				@scope.owner = self
			end
		end
		
		def apply_pass(scope)
			@scope ? @scope : scope
		end
		
		def visit
			@kind_params = yield @kind_params
			@scope = yield @scope if @scope
		end
	end
	
	class TypeFunction < HigherKinded
		attr_accessor :name, :ctype, :type
	
		def initialize(source, name, kind_params, type)
			super(source, kind_params)
			@name = name
			@type = type
		end
		
		def declare_pass(scope)
			super
			@declared = scope.declare(@name, self)
		end
		
		def visit
			super
			@type = yield @type if @type
		end
	end
	
	def self.type_params(ast, plain = true)
		case ast
			when AST::Program
				[]
			when AST::TypeParam, AST::TypeClassInstance, AST::Enum
				plain ? ast.type_params : []
			else
				raise "Missing @declared on #{ast.name}\n#{ast.source.format}" unless ast.declared
				result = type_params(ast.declared.owner)
				result += ast.type_params if plain
				result
		end
	end
	
	class TypeParam < HigherKinded
		attr_accessor :name, :type, :ctype, :value
		
		def initialize(source, name, kind_params, type, value)
			super(source, kind_params)
			@name = name
			@type = type
			@value = value
		end
		
		def declare_pass(scope)
			super
			@declared = scope.declare(@name, self)
		end
		
		def visit
			super
			@type = yield @type if @type
		end
	end
	
	class TypeAlias < HigherKinded
		attr_accessor :name, :type, :ctype
		
		def initialize(source, name, type)
			super(source, AST::KindParams.new(source, [], []))
			@name = name
			@type = type
		end
		
		def declare_pass(scope)
			@declared = scope.declare(@name, self)
		end
		
		def visit
			@type = yield @type
		end
	end
	
	class Complex < HigherKinded
		attr_accessor :name, :scope, :ctype
		
		def parent
			ast = @scope.parent.owner
			ast.is_a?(Program) ? nil : ast
		end
		
		def initialize(source, name, scope, kind_params)
			super(source, kind_params)
			@name = name
			@scope = scope
		end
		
		def declare_pass(scope)
			super
			(@declared = scope.declare(@name, self)) if @name
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
		
		def initialize(source, typeclass, args, scope, kind_params)
			super(source, nil, scope, kind_params)
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
			case @typeclass
				when AST::Ref
					"##{@typeclass.obj.name}_#{__id__}"
				else
					"#TCI_#{@typeclass.__id__}_#{__id__}"
			end
		end
		
		def visit
			super
			@args.map! { |n| yield n }
			@typeclass = yield @typeclass
		end
	end
	
	class Struct < Complex
		attr_accessor :level, :actions, :cases
		
		def initialize(source, name, scope, kind_params, ref = :copyable)
			super(source, name, scope, kind_params)
			@level = ref
			@actions = {}
			@cases = []
		end

		def real_struct
			self
		end

		def enum?
			!cases.empty?
		end
	end
	
	class StructCase < Struct
		attr_accessor :parent
		
		def initialize(*args)
			super
			@level = :opaque
		end

		def real_struct
			@parent
		end

		def declare_pass(scope)
			super
			@parent = scope.owner
			@parent.cases ||= []
			@parent.cases.push(self)
		end
	end
	
	class EnumValue < HigherKinded
		attr_accessor :name, :owner, :ctype
		
		def initialize(source, name, owner)
			super(source, AST::KindParams.new(source, [], []))
			@name = name
			@owner = owner
		end

		def declare_pass(scope)
			@declared = scope.declare(@name, self)
		end
	end
	
	class Enum < Complex
		attr_accessor :values, :actions, :level
		
		def initialize(source, name, values)
			super(source, name, AST::GlobalScope.new(values), AST::KindParams.new(source, [], []))
			@values = values
			@level = :copyable
			@actions = {}
		end
	end
	
	class ActionArgs < Node
		attr_accessor :source, :obj, :action_type
		
		def initialize(source, obj, action_type)
			@source = source
			@obj = obj
			@action_type = action_type
		end
		
		def visit
			@obj = yield @obj
		end
	end

	class LocalScope < Scope
	end
	
	class Function < HigherKinded
		attr_accessor :params, :result, :scope, :type, :ctype, :props, :type_param_count, :action_type, :self, :init_list, :gen_init_list
		attr_writer :name
		
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
					@var = Variable.new(@source, @name, scope, @type, {})
					@declared = scope.declare(@name, @var)
				end
			end
			
			def visit
				@var = yield @var if @var
				@type = yield @type if @type
			end
		end
	
		def initialize(source, name, kind_params)
			super(source, kind_params)
			@props = {}
			@name = name
			@init_list = []
		end
		
		def ref_pass(scope)
			@declared.owner.actions[@action_type] = self if @action_type
		end
		
		def name
			@name ? @name : "action_#{action_type}"
		end
		
		def fowner
			self
		end
		
		def declare_pass(scope)
			super
			@declared = @name ? scope.declare(@name, self) : scope
			@type_param_count = type_params.size

			case scope.owner
				when AST::Struct, AST::TypeClassInstance
					if !props[:shared]
						@self = Variable.new(source, :self, @scope, nil, {})
						@self.declared = @scope.declare(:self, @self)
					end
			end
		end
		
		def visit
			super
			@init_list.map! { |n| yield n }
			@params.map! { |n| yield n }
			@result = yield @result
		end
	end

	class Lambda < Node
		attr_accessor :owner, :fowner, :scope, :params, :gen
		
		def declare_pass(scope)
			@scope.parent = scope
			@scope.owner = self
			raise "Lambda declared outside function in #{scope.owner.scoped_name}\n#{source.format}" unless scope.fscope
			@owner = scope.fscope.owner
		end

		def fowner
			c = owner
			c = c.scope.parent.fscope.owner while !c.is_a?(AST::Function)
			c
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@scope = yield @scope
		end

		def name
			"lambda_#{__id__}"
		end

		def scoped_name
			name
		end
		
		def visit
			super
			@params.map! { |n| yield n }
			@scope = yield @scope
		end
	end

	class ExpressionGroup < Node
		attr_accessor :scope 

		def initialize(source, scope)
			super(source)
			@scope = scope
		end

		def declare_pass(scope)
			@scope.parent = scope
			@scope.owner = self
		end

		def apply_pass(scope)
			@scope
		end
		
		def visit
			@scope = yield @scope
		end
	end
	
	class MatchWhen < Node
		attr_accessor :type, :group
		
		def initialize(source, type, group)
			super(source)
			@type = type
			@group = group
		end

		def visit
			@type = yield @type
			@group = yield @group
		end
	end
	
	class MatchCases < ExpressionNode
		attr_accessor :expr, :binding, :whens, :else_group
		
		def initialize(source, binding_src, binding, whens, else_group)
			super(source)
			@scope = LocalScope.new([])
			@binding = Variable.new(binding_src, binding, @scope, nil, {}, nil)
			@whens = whens
			@else_group = else_group
		end
	
		def declare_pass(scope)
			@scope.parent = scope
			@scope.owner = self
			@scope.declare(binding.name, binding)
		end
		
		def apply_pass(scope)
			@scope
		end
		
		def visit
			@whens.map! { |n| yield n }
			@binding = yield @binding
			@else_group = yield @else_group if @else_group
		end
	end
	
	class MatchAs < ExpressionNode
		attr_accessor :expr, :rest, :gen
		
		def initialize(source, expr, rest)
			super(source)
			@expr = expr
			@rest = rest
		end
	
		def visit
			@expr = yield @expr
			@rest = yield @rest
		end
	end
	
	class Match < ExpressionNode
		attr_accessor :expr, :whens, :else_group, :gen
		
		def initialize(source, expr, whens, else_group)
			super(source)
			@expr = expr
			@whens = whens
			@else_group = else_group
		end
	
		def visit
			@expr = yield @expr
			@whens.map! { |n| yield n }
			@else_group = yield @else_group if @else_group
		end
	end

	class InitEntry < ExpressionNode
		attr_accessor :field, :expr, :gen
		
		def initialize(source, field, expr)
			super(source)
			@field = field
			@expr = expr
		end
	
		def visit
			@expr = yield @expr
		end
	end
	
	class UnaryOp < ExpressionNode
		attr_accessor :op, :node, :gen
		
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
		attr_accessor :lhs, :op, :rhs, :constructing, :gen
		
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
		attr_accessor :name, :value, :var, :gen
		
		def initialize(source, name, type, value, props)
			@source = source
			@name = name
			@props = props
			@var = Variable.new(@source, @name, nil, type, @props, self)
			@value = value
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
		attr_accessor :condition, :group, :else_node, :gen
		
		def initialize(source, condition, group, else_node)
			super(source)
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
		attr_accessor :type, :value, :gen
		
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
		
		def initialize(source, value)
			super(source)
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
	
	class ActionCall < ExpressionNode
		attr_accessor :source, :type, :obj, :action_type, :gen, :arg
		
		def initialize(source, type, obj, action_type, arg = nil)
			@source = source
			@type = type
			@obj = obj
			@action_type = action_type
			@arg = arg
		end
		
		def visit
			@type = yield @type
			@arg = yield @arg
			@obj = yield @obj
		end
	end
end
