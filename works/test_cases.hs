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

	action create

hello[T]()
	.t A2[T]
	return t.field
	
test_hello()
	return hello[int]()
	
class TCool[T]
	type Hi

struct Hi_Type

instance[T] TCool T
	alias Hi = Hi_Type

t_a()
	.var TCool[int].Hi = undef()
	return var

t_b[T]()
	.var TCool[T].Hi = undef()
	return var
	
itest(y)
	y()
	
export itest_e()
	.f () -> bool = undef()
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
	.h List[bool] = undef()
	RecursiveClass.test h

class TypeFuncCool[T]
	type R

struct R_Type

instance TypeFuncCool bool
	alias R = R_Type

TypeFunc_a[T](i T)
	.hello TypeFuncCool[T].R = undef()
	
export TypeFunc_b()
	TypeFunc_a(true)
