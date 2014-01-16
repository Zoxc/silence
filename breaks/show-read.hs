
class Show[T]
	shared show(i T) -> int
	
class Read[T]
	shared read(i int) -> T
	
show[T](i T)
	return Show[T].show(i)
	
read[T](i) -> T
	return Read[T].read(i)
	
f(x)
	return show(read(x))
