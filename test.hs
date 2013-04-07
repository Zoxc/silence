class Cool T, A
	shared hi() -> bool
	hello() -> int

instance[T] Cool T, bool
	shared hi() { return false }
	hello() { return 2 }

a(v: Cool[bool])
	a: int
	r := v.hello()
	return r

import printf(str: *char)

export main()
	a(true)
	Cool[int, bool].hi()
	printf("hello")
	return 0
