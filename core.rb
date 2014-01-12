class Core
	Program = AST::Program.new(AST::Scope.new([]))
	Nodes = Program.scope.nodes
	
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
		
		def func(fname, args, result, params = [])
			r = AST::Function.new(src(2), fname, AST::KindParams.new(src(2), params, []))
			r.params = args.each.map { |name, type| AST::Function::Param.new(src(2), r, name, type) }
			r.result = result
			r.scope = AST::LocalScope.new([])
			r
		end

		def type_func(name, type = nil)
			AST::TypeFunction.new(src(2), name, AST::KindParams.new(src(2), [], []), type ? ref(type) : nil)
		end

		def type_params(obj)
			# TODO: Handle all kinds here
			params = Hash[AST.type_params(obj).map { |tp| [tp, AST::TypeParam.new(tp.source, "P_#{tp.name}".to_sym, AST::KindParams.new(tp.source, [], []), nil, false)] }]
		
			list = [obj]
			current = obj.declared.owner
			while !current.kind_of?(AST::Program)
				list.unshift current
				current = current.declared.owner
			end 
			puts list.map{|n|n.scoped_name}
			wrap_ref = proc do |result, n|
				if n.type_params.empty?
					result
				else	
					AST::Index.new(src, result, n.type_params.map { |tp| ref(params[tp]) })
				end
			end

			first = list.shift

			type_ref = proc do
				list.inject(wrap_ref.(ref(first), first)) do |sum, n|
					wrap_ref.(AST::Field.new(src, sum, n.name), n)
				end
			end

			return params.values, type_ref
		end

		def create_constructor(obj)
			type_params, type_ref = type_params(obj)
			fields = obj.scope.names.values.select { |v| v.is_a?(AST::Variable) && !v.props[:shared] }
			
			r = AST::Function.new(src, :construct, AST::KindParams.new(src, [], []))
			
			args = AST::Tuple.new(src, fields.map { |f| AST::TypeOf.new(src, AST::Field.new(src, type_ref.(), f.name)) })
			args = AST::TypeAlias.new(src, :Args, args)

			constructed = AST::TypeAlias.new(src, :Constructed, type_ref.())
				
			instance = AST::TypeClassInstance.new(src, ref(Core::Constructor::Node), [type_ref.()], AST::GlobalScope.new([r, args, constructed]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', ref(constructed))), AST::Function::Param.new(src, r, :args, ref(args))]
			r.result = ref(Core::Unit)
			fields = fields.map { |field| AST::Field.new(src, AST::UnaryOp.new(src, '*', AST::NameRef.new(src, :obj)), field.name) }
			assign = AST::BinOp.new(src, AST::Tuple.new(src, fields), '=', AST::NameRef.new(src, :args))
			r.scope = AST::LocalScope.new([assign])
			r
		
			instance.run_pass(:declare_pass)
			instance.run_pass(:sema, true)
			instance.run_pass(:ref_pass)
		end

		def create_def_action_constructor(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :construct, AST::KindParams.new(src, [], []))

			instance = AST::TypeClassInstance.new(src, ref(Core::Defaultable::Node), [type_ref.()], AST::GlobalScope.new([r]), AST::KindParams.new(src, type_params, []))
		
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', type_ref.()))]
			r.result = ref(Core::Unit)
			r.scope = AST::LocalScope.new([AST::ActionCall.new(src, AST::NameRef.new(src, :obj), :create)])
			r
		
			instance.run_pass(:declare_pass)
			instance.run_pass(:sema, true)
			instance.run_pass(:ref_pass)
		end
		
	end
	
	# Typeclasses which allows you to increase required levels in type parameters
	
	class Sizeable < Core
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
	Bool = complex :bool
	String = complex :string
	Char = complex :char
	
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

	class Callable < Core
		Args = type_func(:Args, Tuple::Node)
		Result = type_func(:Result, Sizeable::Node)
		
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
		
		Nodes << tci(Callable::Node, [AST::BinOp.new(src, ref(args), '->', ref(result))], [args, result], [apply])
	end.()

	class IntLiteral < Core
		T = param(:T, ref(Sizeable::Node))
		
		Create = func(:create, {input: ref(Int)}, ref(T))
		
		Node = complex(:IntLiteral, [T], AST::TypeClass, [Create])
	end
	
	IntLiterals = {create: {}, default: {}}
	
	num_lit = proc do |type|
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
		
		Join = func(:join, {lhs: ref(T), rhs: ref(T)}, ref(T))
		
		Node = complex(:Joinable, [T], AST::TypeClass, [Join])
	end
	
	OpMap = {'~' => {ref: Joinable::Node, param: Joinable::T, func: Joinable::Join}}

	
	Program.run_pass(:declare_pass, false)
	Program.run_pass(:ref_pass)
end
