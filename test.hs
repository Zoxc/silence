struct Pair A, B
	r: B

class Cool T, A
	shared hi[Y]() -> Pair[A, Y]

instance[Ti] Cool Ti, bool
	shared hi[Yi]()
		v: Pair[bool, Yi]
		return v

export main()
	return Cool[int, bool].hi[int]().r
