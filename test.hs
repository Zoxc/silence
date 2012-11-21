struct A(Hm)
{
	field: Hm
}

hm(v: A(int))
{
	return v.field
}

ok(v)
{
	return v.field
}

test()
{
	v: A
	return ok(v)
}

a(v)
{
	k := v.hi
	1 + id(v.hi.k)
}

id(a)
{
	return a(1)
}
