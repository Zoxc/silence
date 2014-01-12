
a(v Callable, args Tuple)
	return v.apply(args)

test(a int)

export main()
	.args Cell[int, Unit] = Cell(1, Unit())
	
	a(test, args)
	
	return 0
