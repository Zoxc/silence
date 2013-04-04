struct A H
	field: H

	hello[T]()
		t: T
		return field
		
test[Hm](v: A[Hm])
	return v.hello[int]()
	
hm()
	h: A[bool]
	return test(h)