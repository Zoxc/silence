class Core
	Program = AST::Program.new(AST::Scope.new([]))
	Nodes = Program.scope.nodes
	
	class << self
		def complex(name, args = [], klass = AST::Struct, scope = [], ctx = [])
			r = klass.new(src(2), name, AST::GlobalScope.new(scope), AST.kind_params(src(2), args), ctx)
			Nodes << r
			r
		end
		
		def tci(tc, args, params = [], group = [])
			AST::TypeClassInstance.new(src(2), ref(tc), args, AST::GlobalScope.new(group), AST.kind_params(src(2), params), [])
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
			AST::TypeParam.new(src(2), name, AST::RegularKind.new(src(2)), tp)
		end
		
		def func(fname, args, result)
			r = AST::Function.new(src(2), fname)
			r.params = args.each.map { |name, type| AST::Function::Param.new(src(2), r, name, type) }
			r.result = result
			r.scope = AST::LocalScope.new([])
			r
		end

		def type_params_impl(obj, child, prefix, list)
			list = ast.declared.owner
			type_params = obj.kind.params.map { |tp| AST::TypeParam.new(tp.source, "P_#{prefix}#{tp.name}".to_sym, AST::RegularKind.new(tp.source), nil) }
	
			ref = if obj.kind.params.empty?
				ref(obj)
			else
				AST::Index.new(src, ref(obj), type_params.map { |tp| ref(tp) })
			end

			if ast.declared.owner.kind_of?(AST::Program)
				ref
			else
				AST::Field.new(src, ref, )
			end
		end

		def type_params(obj)
			# TODO: Handle all kinds here
			params = Hash[AST.type_params(obj).map { |tp| [tp, AST::TypeParam.new(tp.source, "P_#{tp.name}".to_sym, AST::RegularKind.new(tp.source), nil)] }]
		
			list = [obj]
			current = obj.declared.owner
			while !current.kind_of?(AST::Program)
				list.unshift current
				current = current.declared.owner
			end 
			puts list.map{|n|n.scoped_name}
			wrap_ref = proc do |result, n|
				if n.kind.params.empty?
					result
				else	
					AST::Index.new(src, result, n.kind.params.map { |tp| ref(params[tp]) })
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
			
			r = AST::Function.new(src, :construct)
			
			args = AST::TypeAlias.new(src, :Args, AST::Tuple.new(src, fields.map { |f| AST::TypeOf.new(src, AST::Field.new(src, type_ref.(), f.name)) }))
			constructed = AST::TypeAlias.new(src, :Constructed, type_ref.())
				
			instance = AST::TypeClassInstance.new(src, ref(Core::Constructor::Node), [type_ref.()], AST::GlobalScope.new([r, args, constructed]), AST.kind_params(src, type_params), [])
		
			r.params = [AST::Function::Param.new(src, r, :obj, AST::UnaryOp.new(src, '*', ref(constructed))), AST::Function::Param.new(src, r, :args, ref(args))]
			r.result = ref(Core::Unit)
			fields = fields.map { |field| AST::Field.new(src, AST::UnaryOp.new(src, '*', AST::NameRef.new(src, :obj)), field.name) }
			r.scope = AST::LocalScope.new([AST::BinOp.new(src, AST::Tuple.new(src, fields), '=', AST::NameRef.new(src, :args))])
			r
		
			instance.run_pass(:declare_pass)
			instance.run_pass(:sema, true)
			instance.run_pass(:ref_pass)
		end

		def create_def_action_constructor(obj)
			type_params, type_ref = type_params(obj)

			r = AST::Function.new(src, :construct)

			instance = AST::TypeClassInstance.new(src, ref(Core::Defaultable::Node), [type_ref.()], AST::GlobalScope.new([r]), AST.kind_params(src, type_params), [])
		
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
		Node = complex(:Cell, [Val, Next])
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
		ForceCast = func(:force_cast, {in: ref(ForceCastIn)}, ref(ForceCastOut))
		ForceCast.kind = AST.kind_params(src, [ForceCastOut, ForceCastIn])
		Nodes << ForceCast
	end.()

	class Defaultable < Core
		T = param(:T, ref(Sizeable::Node))
		
		Construct = func(:construct, {obj: ptr(ref(T))}, ref(Unit))
		Construct.props[:shared] = true
		
		Node = complex(:Defaultable, [T], AST::TypeClass, [Construct])
	end

	class Constructor < Core
		Constructed = AST::TypeFunction.new(src, :Constructed, AST.kind_params(src, []), nil)
		Args = AST::TypeFunction.new(src, :Args, AST.kind_params(src, []), ref(Tuple::Node))
		
		Construct = func(:construct, {obj: ptr(ref(Constructed)), args: ref(Args)}, ref(Unit))
		Construct.props[:shared] = true
		
		T = param :T
		Node = complex(:Constructor, [T], AST::TypeClass, [Constructed, Args, Construct])
	end

	class Callable < Core
		Args = AST::TypeFunction.new(src, :Args, AST.kind_params(src, []), ref(Tuple::Node))
		Result = AST::TypeFunction.new(src, :Result, AST.kind_params(src, []), ref(Sizeable::Node))
		
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
