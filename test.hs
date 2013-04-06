struct A T
	shared t: T
	
	k: bool
	
	tmpl[A](t)
		a: A
		return t
	
	hi()
		return k
	
h: A[int]

b(y: A)
	return y.tmpl[bool](1)

a()
	return b(h)
	
import printf(str: *char)

export t()
	h.hi()
	return a()
	
export main()
	printf("hello")
	return 0
