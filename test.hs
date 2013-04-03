
class Cool T, A
	test: (A) -> int
	hello()
		return test

instance[T] Cool T, bool
	test: (bool) -> int
	hello() { return test }

a(v: Cool[bool])
	a: int
	r := v.hello()
	return r(true)
	