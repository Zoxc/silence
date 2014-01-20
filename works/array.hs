import malloc(size uint) -> *()
import free(mem *())
import memcpy(dst *(), src *(), len uint)

struct Array[T]
	list *T
	size uint

	shared unit = size_of[T]()

	action create
		size = 0
		list = force_cast malloc(1)
	
	action copy
		.b = size * unit
		.new = malloc(b)
		memcpy(new, force_cast list, b)
		list = force_cast new

	action destroy
		free(force_cast list)

	ptr(idx uint) -> *T
		return force_cast(force_cast list + idx * size_of[T]())

	get(idx uint)
		return *ptr(idx)

instance[T] Callable Array[T]
	alias Result = T
	alias Args = Cell[uint, Unit]

	apply(args Cell[uint, Unit]) -> T
		return self.get(args.val)

instance[T] Indexable Array[T]
	alias Result = T
	alias Index = Cell[uint, Unit]

	ref(args Cell[uint, Unit])
		return self.ptr(args.val)

export main() -> c_int
	.v Array[c_int]
	v(0)
	v(1) = 2
	.a = v
	return 0