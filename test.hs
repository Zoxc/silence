import printf(format: *char): unit

b()
{
	return a()
}

c()
{
	return b()
}

a()
{
	return c()
}

h()
{
	a := 1
	a = a + 3 + 1
	return a
}

export main(): unit
{
	printf("Hello there")
	
	if true
		printf("Cool")
	else
		printf("Odd")
}
