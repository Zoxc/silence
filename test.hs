struct A(Hm)
{
	field: Hm
	
	read(v)
	{
		v.hello
		return field
	}
}

hm(v: A(int))
{
	return v.read
}

ok(v)
{
	return v.read
}

test()
{
	v: A
	return ok(v)
}
