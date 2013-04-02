class Cool T, A
	test: (A) -> int
	hello()
		return test

instance[T] Cool T, bool

a(v: Cool, t)
	a: int
	r := v.hello()
	return r(t)
