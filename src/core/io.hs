struct IO
	shared print(s String)
		s.each |c| putchar(force_cast c)

	shared puts(s String)
		print(s)
		putchar(10)