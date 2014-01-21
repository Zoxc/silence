struct Union
	common uint

	when Extra
		val uint
	when Empty

export main() -> c_int
	.a = Union.Extra(2, 4)
	a.common = 4
	a = Union.Empty(3)

	match a as a
		when Union.Extra
			1
		when Union.Empty
			2
		else
			5

	return 0