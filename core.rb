class Core
	Nodes = []
	Generated = AST::Scope.new([])

	class << self
		def complex(name, args = [], klass = AST::Struct, scope = [], ctx = [])
			r = klass.new(src(2), name, AST::GlobalScope.new(scope), AST::KindParams.new(src(2), args, ctx))
			Nodes << r
			create_empty_action(r, :copy, false) if r.is_a?(AST::Struct)
			r
		end

		def base(type, *args)
			r = args.inject(Silence.get_program) do |m, v|
				m.scope.require(src(5), v)
			end
			raise "Expected #{type}, but got #{r.class}" unless r.is_a?(type)
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
			r.scope = AST::FuncScope.new([])
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

		def create_empty_action(obj, name, pass = true)
			r = AST::Function.new(src, nil, AST::KindParams.new(src, [], []))
			r.action_type = name
			r.params = []
			r.result = ref(Unit)
			r.scope = AST::FuncScope.new([])
			obj.scope.nodes << r

			obj.actions[name] = r

			run_pass(r, obj.scope) if pass
		end

		def create_constructor_action(obj)
			scope = obj.scope.names.values
			(scope = obj.parent.scope.names.values + scope) if obj.is_a?(AST::StructCase)
			fields = scope.select { |v| v.is_a?(AST::Variable) && !v.props[:shared] }

			r = AST::Function.new(src, nil, AST::KindParams.new(src, [], []))
			r.action_type = :create_args
			r.params = fields.each_with_index.map do |field, i|
				AST::Function::Param.new(src, r, field.name, nil)
			end
			r.result = ref(Unit)
			r.init_list = fields.each_with_index.map do |field, i|
				AST::InitEntry.new(src, field.name, AST::NameRef.new(src, field.name))
			end
			r.scope = AST::FuncScope.new([])
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
			r.result = ref(Unit)
			r.scope = AST::FuncScope.new([AST::ActionCall.new(src, type_ref.(), AST::NameRef.new(src, :obj), :create_args, AST::NameRef.new(src, :args))])
			r.props[:shared] = true

			instance = AST::TypeClassInstance.new(src, ref(Constructor::Node), [type_ref.()], AST::GlobalScope.new([r, args, constructed]), AST::KindParams.new(src, type_params, []))
			Generated.nodes << instance
			run_pass(instance)
		end

		def create_def_constructor(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :construct, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Defaultable::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', type_ref.()))]
			r.result = ref(Unit)
			r.scope = AST::FuncScope.new([AST::ActionCall.new(src, type_ref.(), AST::NameRef.new(src, :obj), :create)])
			r.props[:shared] = true

			Generated.nodes << instance
			run_pass(instance)
		end
		
		def create_enum_eq(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :equal, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Eq::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :lhs, type_ref.()), AST::Function::Param.new(src, r, :rhs, type_ref.())]
			r.result = ref(Bool)

			lhs = AST::Call.new(src, AST::Index.new(src, ref(ForceCast), [ref(UInt)]), [AST::NameRef.new(src, :lhs)])
			rhs = AST::Call.new(src, ref(ForceCast), [AST::NameRef.new(src, :rhs)])

			r.scope = AST::FuncScope.new([AST::Return.new(src, AST::BinOp.new(src, lhs, '==', rhs))])
			r.props[:shared] = true

			Generated.nodes << instance
			run_pass(instance)
		end
		
		def create_enum_str(obj)
			r = AST::Function.new(src, :str, AST::KindParams.new(src, [], []))
			r.params = []
			cases = obj.values.map do |v|
				AST::MatchWhen.new(src, [AST::Ref.new(src, v)], AST::LocalScope.new([AST::Literal.new(src, :string, v.name.to_s)]))
			end
			match = AST::Match.new(src, AST::NameRef.new(src, :self), cases, nil)
			r.scope = AST::FuncScope.new([AST::Return.new(src, match)])
			obj.scope.nodes << r

			run_pass(r, obj.scope)
		end

		def create_sizable(obj)
			type_params, type_ref = type_params(obj)

			instance = AST::TypeClassInstance.new(src, ref(Sizeable::Node), [type_ref.()], AST::GlobalScope.new([]), AST::KindParams.new(src, type_params, []))
		
			Generated.nodes << instance
			run_pass(instance)
		end
		
		def create_copyable(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :copy, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Copyable::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
			r.params = []
			r.result = ref(Unit)
			r.scope = AST::FuncScope.new([AST::ActionCall.new(src, type_ref.(), AST::UnaryOp.new(src, '&', AST::NameRef.new(src, :self)), :copy)])

			Generated.nodes << instance
			run_pass(instance)
		end
		
	end

	class Sizeable < Core # TODO: Rename this
		T = base AST::TypeParam, :Sizeable, :T
		Node = base AST::TypeClass, :Sizeable
	end
	
	class Copyable < Core
		T = base AST::TypeParam, :Copyable, :T
		Node = base AST::TypeClass, :Copyable
	end
	
	Unit = base AST::Struct, :Unit
	
	class Tuple < Core
		T = base AST::TypeParam, :Tuple, :T
		Node = base AST::TypeClass, :Tuple
	end
	
	class Cell < Core
		Val = base AST::TypeParam, :Cell, :Val
		Next = base AST::TypeParam, :Cell, :Next
		Node = base AST::Struct, :Cell
	end
	
	class Option < Core
		T = base AST::TypeParam, :Option, :T
		Node = base AST::Struct, :Option
		None = base AST::StructCase, :Option, :None
	end
	
	class Func < Core
		Args = param(:Args, ref(Tuple::Node))
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
	Char = complex :char

	proc do
		values = []
		Bool = AST::Enum.new(src, :bool, [])
		values << AST::EnumValue.new(src, :false, Bool)
		values << AST::EnumValue.new(src, :true, Bool)
		Bool.values = values
		Nodes << Bool
		Nodes.concat(values)
	end.()

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

	proc do
		t = param :T
		destroy = func(:destroy, {obj: ref(t)}, ref(Unit), [t])
		destroy.scope.nodes << AST::ActionCall.new(src, ref(t), AST::NameRef.new(src, :obj), :destroy)
		Nodes << destroy
	end.()
	
	class Defaultable < Core
		T = base AST::TypeParam, :Defaultable, :T
		Construct = base AST::Function, :Defaultable, :construct
		Node = base AST::TypeClass, :Defaultable
	end

	class Constructor < Core
		Constructed = base AST::TypeFunction, :Constructor, :Constructed
		Args = base AST::TypeFunction, :Constructor, :Args
		Construct = base AST::Function, :Constructor, :construct
		T = base AST::TypeParam, :Constructor, :T
		Node = base AST::TypeClass, :Constructor
	end

	class Reference < Core
		Type = base AST::TypeFunction, :Reference, :Type
		Get = base AST::Function, :Reference, :get
		T = base AST::TypeParam, :Reference, :T
		Node = base AST::TypeClass, :Reference
	end

	class Callable < Core
		Args = base AST::TypeFunction, :Callable, :Args
		Result = base AST::TypeFunction, :Callable, :Result
		Apply = base AST::Function, :Callable, :apply
		T = base AST::TypeParam, :Callable, :T
		Node = base AST::TypeClass, :Callable
	end

	class Indexable < Core
		Index = base AST::TypeFunction, :Indexable, :Index
		Result = base AST::TypeFunction, :Indexable, :Result
		Ref = base AST::Function, :Indexable, :ref
		T = base AST::TypeParam, :Indexable, :T
		Node = base AST::TypeClass, :Indexable
	end

	class Eq < Core
		T = base AST::TypeParam, :Eq, :T
		Equal = base AST::Function, :Eq, :equal
		Node = base AST::TypeClass, :Eq
	end

	Order = base AST::Enum, :Order

	class Ord < Core
		T = base AST::TypeParam, :Ord, :T
		Cmp = base AST::Function, :Ord, :cmp
		Node = base AST::TypeClass, :Ord
	end

	class Num < Core
		T = base AST::TypeParam, :Num, :T
		Create = base AST::Function, :Num, :create
		Neg = base AST::Function, :Num, :neg
		Add = base AST::Function, :Num, :add
		Sub = base AST::Function, :Num, :sub
		Mul = base AST::Function, :Num, :mul
		Div = base AST::Function, :Num, :div
		Mod = base AST::Function, :Num, :mod
		Node = base AST::TypeClass, :Num
	end

	class Bits < Core
		T = base AST::TypeParam, :Bits, :T
		Neg = base AST::Function, :Bits, :neg
		Xor = base AST::Function, :Bits, :xor
		And = base AST::Function, :Bits, :and
		Or = base AST::Function, :Bits, :or
		ShL = base AST::Function, :Bits, :shl
		ShR = base AST::Function, :Bits, :shr
		Node = base AST::TypeClass, :Bits
	end

	proc do
		args = param :Args
		result = param :Result
		
		apply = func(:apply, {args: ref(args)}, ref(result))
		
		CallableFuncArgs = args
		CallableFuncApply = apply
		
		Nodes << tci(Callable::Node, [AST::Index.new(src, ref(Func::Node), [ref(args), ref(result)])], [args, result], [apply])
	end.()

	proc do
		_args = param :Args
		_result = param :Result
		_func = proc { AST::Index.new(src, ref(Func::Node), [ref(_args), ref(_result)]) }
		FuncEq = func(:equal, {lhs: _func.(), rhs: _func.()}, ref(Bool), [], true)
		inst = tci(Eq::Node, [_func.()], [_args, _result], [FuncEq])
		Nodes << inst
	end.()

	IntLiterals = {create: {}, default: {}, eq: {}, ord: {}, num: {}, num_not: {}, bits: {}, bits_other: {}}
	
	num_lit = proc do |type|
		create = func(:create, {i: ref(Int)}, ref(type))
		neg = func(:neg, {}, ref(type))
		add = func(:add, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		sub = func(:sub, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		mul = func(:mul, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		div = func(:div, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		mod = func(:mod, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		num_inst = tci(Num::Node, [ref(type)], [], [neg, add, sub, mul, div, mod, create])
		Nodes << num_inst
		IntLiterals[:create][create] = create
		IntLiterals[:num_not][neg] = neg
		IntLiterals[:num][add] = add
		IntLiterals[:num][sub] = sub
		IntLiterals[:num][mul] = mul
		IntLiterals[:num][div] = div
		IntLiterals[:num][mod] = mod
		
		bits_neg = func(:neg, {}, ref(type))
		xor = func(:xor, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		_or = func(:or, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		_and = func(:and, {lhs: ref(type), rhs: ref(type)}, ref(type), [], true)
		shl = func(:shl, {lhs: ref(type), rhs: ref(UInt)}, ref(type), [], true)
		shr = func(:shr, {lhs: ref(type), rhs: ref(UInt)}, ref(type), [], true)
		bits_inst = tci(Bits::Node, [ref(type)], [], [bits_neg, xor, _and, _or, shl, shr])
		Nodes << bits_inst
		IntLiterals[:bits_other][bits_neg] = bits_neg
		IntLiterals[:bits][xor] = xor
		IntLiterals[:bits][_or] = _or
		IntLiterals[:bits][_and] = _and
		IntLiterals[:bits_other][shl] = shl
		IntLiterals[:bits_other][shr] = shr
		
		cmp = func(:cmp, {gt: ref(type), ls: ref(type)}, ref(Order), [], true)
		ord_inst = tci(Ord::Node, [ref(type)], [], [cmp])
		Nodes << ord_inst
		IntLiterals[:ord][cmp] = cmp
		
		equal = func(:equal, {lhs: ref(type), rhs: ref(type)}, ref(Bool), [], true)
		eq_inst = tci(Eq::Node, [ref(type)], [], [equal])
		Nodes << eq_inst
		IntLiterals[:eq][equal] = equal
		
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
		T = base AST::TypeParam, :StringLiteral, :T
		Create = base AST::Function, :StringLiteral, :create
		Node = base AST::TypeClass, :StringLiteral
	end
	
	class Joinable < Core
		T = base AST::TypeParam, :Joinable, :T
		Join = base AST::Function, :Joinable, :join
		Node = base AST::TypeClass, :Joinable
	end

	UnaryOpMap = {
		'-' => {ref: Num::Node, param: Num::T, func: Num::Neg},
		'~' => {ref: Bits::Node, param: Bits::T, func: Bits::Neg}
	}

	OpMap = {
		'+' => {ref: Num::Node, param: Num::T, func: Num::Add},
		'-' => {ref: Num::Node, param: Num::T, func: Num::Sub},
		'*' => {ref: Num::Node, param: Num::T, func: Num::Mul},
		'/' => {ref: Num::Node, param: Num::T, func: Num::Div},
		'%' => {ref: Num::Node, param: Num::T, func: Num::Mod},
		'^' => {ref: Bits::Node, param: Bits::T, func: Bits::Xor},
		'&' => {ref: Bits::Node, param: Bits::T, func: Bits::And},
		'|' => {ref: Bits::Node, param: Bits::T, func: Bits::Or},
		'<<' => {ref: Bits::Node, param: Bits::T, func: Bits::ShL, rhs: UInt},
		'>>' => {ref: Bits::Node, param: Bits::T, func: Bits::ShR, rhs: UInt},
		'>' => {ref: Ord::Node, param: Ord::T, func: Ord::Cmp, result: Bool},
		'~' => {ref: Joinable::Node, param: Joinable::T, func: Joinable::Join},
		'==' => {ref: Eq::Node, param: Eq::T, func: Eq::Equal, result: Bool}
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
			_index = AST::TypeAlias.new(src, :Index, AST::TypeTuple.new(src, [ref(UInt)]))
			_result = AST::TypeAlias.new(src, :Result, ref(_type))
			Ref = func(:ref, {index: ref(_index)}, ptr(ref(_result)))
			inst = tci(Indexable::Node, [AST::Index.new(src, ref(Node), [ref(_size), ref(_type)])], [_size, _type], [Ref, _index, _result])
			Nodes << inst
		end.()
	end

	Nodes.each do |n|
		n.run_pass(:declare_pass, false, Silence.get_program.scope)
	end
end
