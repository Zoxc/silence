struct Union
	common uint

	when Extra
		val uint
	when Empty

export main() -> c_int
	.a = Union.Extra(2, 4)
	a.common = 4
	a = Union.Empty(3)
	return 0