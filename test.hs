import printf(format: *char): unit

b()
{
	a()
}

c()
{
	b()
}

a()
{
	c()
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
	
	if 1
		printf("Cool")
	else
		printf("Odd")
}
