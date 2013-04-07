struct Pair A, B
	r: B

class Cool T, A
	shared hi[Y]() -> Pair[A, Y]

instance[T] Cool T, bool
	shared hi[Y]()
		v: Pair[bool, Y]
		return v

export main()
	return Cool[int, bool].hi[int]().r
