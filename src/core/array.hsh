struct Array(T)
	list *T
	size uint

	shared unit = size_of[T]()

	action create
		size = 0
		list = force_cast malloc(0)
	
	action copy
		.b = size * unit
		.new = malloc(b)
		memcpy(new, force_cast list, b)
		list = force_cast new

	action destroy
		free(force_cast list)

	ptr(idx uint) -> *T
		return ptr_idx(list, idx)

	get(idx uint)
		return *ptr(idx)

instance(T) Callable(Array(T))
	alias Result = T
	alias Args = Cell(uint, Unit)

	apply(args Cell(uint, Unit))
		return self.get(args.val)

instance(T) Indexable(Array(T))
	alias Result = T
	alias Index = Cell(uint, Unit)

	ref(args Cell(uint, Unit))
		return self.ptr(args.val)
