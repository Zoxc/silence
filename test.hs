struct B
{
	test: int
}

class Cool T, A
{
	test: A
	hello() { return test }
}

cool(v: Cool[int])
{
	return v.hello()
}

struct A Hm
{
	field: Hm
	
	read()
	{
		return field
	}
}

hm(v: A)
{
	return v.read
}