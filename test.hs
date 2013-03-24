class Cool T, A
	test: (A) -> int
	hello()
		return test

cool(v: Cool[int])
	a: int
	return a = v.hello(a)

callable(s: int, v: bool)
	return 1

test(func, a)
	return func(1, true) + a

test2()
	return test(callable, 2)

struct A Hm
	field: Hm
	
	read()
		return field

hm(v: A)
	return v.read