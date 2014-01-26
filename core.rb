class Core
	Program = AST::Program.new(AST::Scope.new([]))
	Nodes = Program.scope.nodes
	Generated = AST::Scope.new([])

	class << self
		def complex(name, args = [], klass = AST::Struct, scope = [], ctx = [])
			r = klass.new(src(2), name, AST::GlobalScope.new(scope), AST::KindParams.new(src(2), args, ctx))
			Nodes << r
			r
		end
		
		def tci(tc, args, params = [], group = [])
			AST::TypeClassInstance.new(src(2), ref(tc), args, AST::GlobalScope.new(group), AST::KindParams.new(src(2), params, []))
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
			AST::TypeParam.new(src(2), name, AST::KindParams.new(src(2), [], []), tp, false)
		end
		
		def func(fname, args, result, params = [], shared = false)
			r = AST::Function.new(src(2), fname, AST::KindParams.new(src(2), params, []))
			r.params = args.each.map { |name, type| AST::Function::Param.new(src(2), r, name, type) }
			r.result = result
			r.scope = AST::LocalScope.new([])
			r.props = {shared: shared}
			r
		end

		def type_func(name, type = nil)
			AST::TypeFunction.new(src(2), name, AST::KindParams.new(src(2), [], []), type ? ref(type) : nil)
		end

		def type_params(obj)
			# TODO: Handle all kinds here
			params = Hash[AST.type_params(obj).map { |tp| [tp, AST::TypeParam.new(src, "P_#{tp.name}".to_sym, AST::KindParams.new(tp.source, [], []), nil, false)] }]
		
			# TODO: Disallow structs in typeclass instances. They have no owner

			list = [obj]
			current = obj.declared.owner
			while !current.kind_of?(AST::Program)
				list.unshift current
				current = current.declared.owner
			end 

			wrap_ref = proc do |result, n|
				if n.type_params.empty?
					result
				else	
					AST::Index.new(src, result, n.type_params.map { |tp| ref(params[tp]) })
				end
			end

			first = list.shift

			type_ref = proc do |list|
				list.inject(wrap_ref.(ref(first), first)) do |sum, n|
					wrap_ref.(AST::Field.new(src, sum, n.name), n)
				end
			end

			return params.values, proc { type_ref.(list) }, proc { type_ref.(list[0..-2]) }
		end

		def run_pass(ast, scope = nil)
			ast.run_pass(:declare_pass, false, scope)
			ast.run_pass(:sema, true)
			ast.run_pass(:ref_pass)
		end

		def create_constructor_action(obj)
			scope = obj.scope.names.values
			(scope = obj.parent.scope.names.values + scope) if obj.is_a?(AST::StructCase)
			fields = scope.select { |v| v.is_a?(AST::Variable) && !v.props[:shared] }

			args = AST::Tuple.new(src, fields.map { |f| AST::TypeOf.new(src, AST::NameRef.new(src, f.name)) })
			
			# TODO: This should not destroy existing fields
			# TODO: Make sure there are no conflicts with names

			r = AST::Function.new(src, nil, AST::KindParams.new(src, [], []))
			r.action_type = :create_args
			r.params = [AST::Function::Param.new(src, r, :args, args)]
			r.result = ref(Core::Unit)
			fields = fields.map { |field| AST::NameRef.new(src, field.name) }
			assign = AST::BinOp.new(src, AST::Tuple.new(src, fields), '=', AST::NameRef.new(src, :args))
			r.scope = AST::FuncScope.new([assign])
			obj.scope.nodes << r

			run_pass(r, obj.scope)
		end

		def create_constructor(obj)
			eparent = obj.parent if obj.is_a?(AST::StructCase)

			type_params, type_ref, parent_ref = type_params(obj)

			args = AST::TypeAlias.new(src, :Args, AST::ActionArgs.new(src, type_ref.(), :create_args))

			constructed = AST::TypeAlias.new(src, :Constructed, eparent ? parent_ref.() : type_ref.())
			
			r = AST::Function.new(src, :construct, AST::KindParams.new(src, [], []))
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', ref(constructed))), AST::Function::Param.new(src, r, :args, ref(args))]
			r.result = ref(Core::Unit)
			r.scope = AST::FuncScope.new([AST::ActionCall.new(src, type_ref.(), AST::NameRef.new(src, :obj), :create_args, AST::NameRef.new(src, :args))])
			r.props[:shared] = true

			instance = AST::TypeClassInstance.new(src, ref(Core::Constructor::Node), [type_ref.()], AST::GlobalScope.new([r, args, constructed]), AST::KindParams.new(src, type_params, []))
			Generated.nodes << instance
			run_pass(instance)
		end

		def create_def_constructor(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :construct, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Core::Defaultable::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', type_ref.()))]
			r.result = ref(Core::Unit)
			r.scope = AST::FuncScope.new([AST::ActionCall.new(src, type_ref.(), AST::NameRef.new(src, :obj), :create)])
			r.props[:shared] = true

			Generated.nodes << instance
			run_pass(instance)
		end
		
		def create_enum_eq(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :equal, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Core::Eq::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :lhs, type_ref.()), AST::Function::Param.new(src, r, :rhs, type_ref.())]
			r.result = ref(Core::Bool)

			lhs = AST::Call.new(src, AST::Index.new(src, ref(Core::ForceCast), [ref(Core::UInt)]), [AST::NameRef.new(src, :lhs)])
			rhs = AST::Call.new(src, ref(Core::ForceCast), [AST::NameRef.new(src, :rhs)])

			r.scope = AST::FuncScope.new([AST::Return.new(src, AST::BinOp.new(src, lhs, '==', rhs))])
			r.props[:shared] = true

			Generated.nodes << instance
			run_pass(instance)
		end
	end
	
	# Typeclasses which allows you to increase required levels in type parameters
	
	class Sizeable < Core # TODO: Rename this
		T = param :T
		Node = complex(:Sizeable, [T], AST::TypeClass)
		
		t = param :T
		Instance = tci(Sizeable::Node, [ref(t)], [t])
		Nodes << Instance
	end
	
	class Copyable < Core
		T = param :T
		Node = complex(:Copyable, [T], AST::TypeClass)
		
		t = param :T
		Instance = tci(Copyable::Node, [ref(t)], [t])
		Nodes << Instance
	end
	
	
	Unit = complex :Unit
	
	class Tuple < Core
		T = param(:T, ref(Copyable::Node))
		Node = complex(:Tuple, [T], AST::TypeClass)
	end
	
	class Cell < Core
		Val = param(:Val, ref(Copyable::Node))
		Next = param(:Next, ref(Tuple::Node))
		fval = AST::VariableDecl.new(src, :val, ref(Val), nil, {})
		fnext = AST::VariableDecl.new(src, :next, ref(Next), nil, {})
		Node = complex(:Cell, [Val, Next], AST::Struct, [fval, fnext])
	end
	
	Nodes << tci(Tuple::Node, [ref(Unit)])

	proc do
		val = param :Val
		_next = param :Next
		Nodes << tci(Tuple::Node, [AST::Index.new(src, ref(Cell::Node), [ref(val), ref(_next)])], [val, _next])
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
	CInt = complex :c_int

	proc do
		values = []
		Bool = AST::Enum.new(src, :bool, [])
		values << AST::EnumValue.new(src, :false, Bool)
		values << AST::EnumValue.new(src, :true, Bool)
		Bool.values = values
		Nodes << Bool
		Nodes.concat(values)
	end.()

	String = complex :string
	Char = complex :char
	
	proc do
		SizeOfT = param(:T, ref(Sizeable::Node))
		SizeOf = func(:size_of, {}, ref(UInt), [SizeOfT])
		Nodes << SizeOf
	end.()

	proc do
		UndefT = param :T
		Undef = func(:undef, {}, ref(UndefT), [UndefT])
		Nodes << Undef
	end.()

	proc do
		ForceCastIn = param :In
		ForceCastOut = param :Out
		ForceCast = func(:force_cast, {in: ref(ForceCastIn)}, ref(ForceCastOut), [ForceCastOut, ForceCastIn])
		Nodes << ForceCast
	end.()

	class Defaultable < Core
		T = param(:T, ref(Sizeable::Node))
		
		Construct = func(:construct, {obj: ptr(ref(T))}, ref(Unit))
		Construct.props[:shared] = true
		
		Node = complex(:Defaultable, [T], AST::TypeClass, [Construct])
	end

	class Constructor < Core
		Constructed = type_func :Constructed
		Args = type_func(:Args, Tuple::Node)
		
		Construct = func(:construct, {obj: ptr(ref(Constructed)), args: ref(Args)}, ref(Unit))
		Construct.props[:shared] = true
		
		T = param :T
		Node = complex(:Constructor, [T], AST::TypeClass, [Constructed, Args, Construct])
	end

	class Reference < Core
		Type = type_func(:Type, Sizeable::Node)
		
		Get = func(:get, {}, ptr(ref(Type)))

		T = param :T
		Node = complex(:Reference, [T], AST::TypeClass, [Type, Get])
	end

	class Callable < Core
		Args = type_func(:Args, Tuple::Node)
		Result = type_func(:Result, Sizeable::Node)
		
		Apply = func(:apply, {args: ref(Args)}, ref(Result))
		
		T = param :T
		Node = complex(:Callable, [T], AST::TypeClass, [Args, Result, Apply])
	end

	class Indexable < Core
		Index = type_func(:Index, Tuple::Node)
		Result = type_func(:Result, Sizeable::Node)
		
		Ref = func(:ref, {index: ref(Index)}, ptr(ref(Result)))
		
		T = param :T
		Node = complex(:Indexable, [T], AST::TypeClass, [Index, Result, Ref])
	end

	class Eq < Core
		T = param :T
		Equal = func(:equal, {lhs: ref(T), rhs: ref(T)}, ref(Bool), [], true)
		Node = complex(:Eq, [T], AST::TypeClass, [Equal])
	end

	proc do
		values = []
		Order = AST::Enum.new(src, :Order, values)
		values << AST::EnumValue.new(src, :Greater, Order)
		values << AST::EnumValue.new(src, :Equal, Order)
		values << AST::EnumValue.new(src, :Lesser, Order)
		Nodes << Order
	end.()

	class Ord < Core
		T = param :T
		Cmp = func(:cmp, {gt: ref(T), ls: ref(T)}, ref(Order), [], true)
		Node = complex(:Ord, [T], AST::TypeClass, [Cmp])
	end

	proc do
		args = param :Args
		result = param :Result
		
		apply = func(:apply, {args: ref(args)}, ref(result))
		
		CallableFuncArgs = args
		CallableFuncApply = apply
		
		Nodes << tci(Callable::Node, [AST::BinOp.new(src, ref(args), '->', ref(result))], [args, result], [apply])
	end.()

	class IntLiteral < Core
		T = param(:T, ref(Sizeable::Node))
		
		Create = func(:create, {input: ref(Int)}, ref(T))
		
		Node = complex(:IntLiteral, [T], AST::TypeClass, [Create])
	end
	
	IntLiterals = {create: {}, default: {}, eq: {}, ord: {}}
	
	num_lit = proc do |type|
		cmp = func(:cmp, {gt: ref(type), ls: ref(type)}, ref(Order), [], true)
		ord_inst = tci(Ord::Node, [ref(type)], [], [cmp])
		Nodes << ord_inst
		IntLiterals[:ord][cmp] = cmp
		
		equal = func(:equal, {lhs: ref(type), rhs: ref(type)}, ref(Bool), [], true)
		eq_inst = tci(Eq::Node, [ref(type)], [], [equal])
		Nodes << eq_inst
		IntLiterals[:eq][equal] = equal
		
		create = func(:create, {input: ref(Int)}, ref(type))
		create_inst = tci(IntLiteral::Node, [ref(type)], [], [create])
		Nodes << create_inst
		IntLiterals[:create][create] = create
		
		construct = func(:construct, {obj: ptr(ref(type))}, ref(Unit))
		default_inst = tci(Defaultable::Node, [ref(type)], [], [construct])
		Nodes << default_inst
		IntLiterals[:default][construct] = construct
	end
	
	num_lit.(Char)
	num_lit.(CInt)
	num_lit.(Int)
	num_lit.(UInt)
	
	class StringLiteral < Core
		T = param(:T, ref(Sizeable::Node))
		
		Create = func(:create, {data: ptr(ref(Char)), length: ref(UInt)}, ref(T))
		
		Node = complex(:StringLiteral, [T], AST::TypeClass, [Create])
	end
	
	class Joinable < Core
		T = param :T
		
		Join = func(:join, {lhs: ref(T), rhs: ref(T)}, ref(T), [], true)
		
		Node = complex(:Joinable, [T], AST::TypeClass, [Join])
	end

	AssignOps = ['+=', '-=', '*=', '/=', '%=', '~=', '=']
	
	OpMap = {
		'>' => {ref: Ord::Node, param: Ord::T, func: Ord::Cmp, result: Core::Bool},
		'~' => {ref: Joinable::Node, param: Joinable::T, func: Joinable::Join},
		'==' => {ref: Eq::Node, param: Eq::T, func: Eq::Equal, result: Core::Bool}
	}
	OpMap['!='] = OpMap['==']
	OpMap['>='] = OpMap['>']
	OpMap['<='] = OpMap['>']
	OpMap['<'] = OpMap['>']

	class Table < Core
		Type = param(:Type, ref(Copyable::Node))
		Size = AST::TypeParam.new(src, :Size, AST::KindParams.new(src, [], []), ref(UInt), true)
		Node = complex(:Table, [Size, Type])

		proc do
			_type = param :Type
			_size = AST::TypeParam.new(src, :Size, AST::KindParams.new(src, [], []), ref(UInt), true)
			_index = AST::TypeAlias.new(src, :Index, AST::Tuple.new(src, [ref(UInt)]))
			_result = AST::TypeAlias.new(src, :Result, ref(_type))
			Ref = func(:ref, {index: ref(_index)}, ptr(ref(_result)))
			inst = tci(Indexable::Node, [AST::Index.new(src, ref(Node), [ref(_size), ref(_type)])], [_size, _type], [Ref, _index, _result])
			Nodes << inst
		end.()
	end
	
	Program.run_pass(:declare_pass, false)
	Program.run_pass(:ref_pass)
end
