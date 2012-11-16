struct A(Hm)
{
	field: Hm
}

hm(v: A(int))
{
	return v.field
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
