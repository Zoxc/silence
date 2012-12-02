struct B
{
	test: int
}

struct A(Hm)
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