struct A[T]
	shared t T
	
	k int
	
	tmpl[A](t)
		.a A
		return t
	
	hi()
		return k
	
h A[int]

b(y A)
	return y.tmpl[int](1)

a()
	return b(h)
	
instance StringLiteral *char
	create(data *char, length uint)
		return data
	
import printf(str *char)

export t()
	h.hi()
	return a()
	
export main()
	printf("hello")
	return 0
