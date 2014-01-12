struct A[X]
	a X
	
	struct B[Y]
		a X
		b Y
		t1 B[X]
		
		struct C[Z]
			a X
			b Y
			c Z
			t2 C[Y]
		
a(i A.B.C)
	return i.t2

func1(i A.B)
	return i.t1

	
callable(s int, v bool)
	return 1

test(func, a)
	return func(1, true) + a

test2()
	return test(callable, 2)

class Cool[T, A]
	test (A) -> int
	hello()
		return test

instance[T] Cool T, bool
	test (bool) -> int
	hello() { return test }

a2(v Cool[bool])
	.a int
	.r = v.hello()
	return r(true)

struct A2[H]
	field H

hello[T]()
	.t A2[T]
	return t.field
	
test_hello()
	return hello[int]()
	
class TCool[T]
	type Hi

instance[T] TCool T
	struct Hi

t_a()
	.var TCool[int].Hi
	return var

t_b[T]()
	.var TCool[T].Hi
	return var
	
itest(y)
	y()
	
export itest_e()
	.f () -> bool
	itest(f)
	
class RecursiveClass[T]
	shared test(v T) -> int

instance RecursiveClass bool
	shared test(v bool) -> int
		return 1

struct List[T]
	field T

instance[T RecursiveClass] RecursiveClass List[T]
	shared test(v List[T])
		return RecursiveClass.test(v.field)

RecursiveClass_a()
	.h List[bool]
	RecursiveClass.test h

class TypeFuncCool[T]
	type R

instance TypeFuncCool bool
	struct R

TypeFunc_a[T](i T)
	.hello TypeFuncCool[T].R
	
export TypeFunc_b()
	TypeFunc_a(true)
