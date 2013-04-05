struct A T
	shared t: T
	
	k: bool
	
	tmpl[A](t)
		a: A
		return t
	
	hi()
		return t
	
h: A[int]

b(y: A)
	return y.tmpl[bool](1)

a()
	return b(h)

export t()
	return a()
