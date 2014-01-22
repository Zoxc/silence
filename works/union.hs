import puts(str *char)
import malloc(size uint) -> *()
import memcpy(dst *(), src *(), len uint)

instance StringLiteral *char
	create(data *char, size)
		.new = malloc(size + 1)
		memcpy(new, force_cast data, size)
		return force_cast[*char] new

struct Union[T]
	common uint

	when Extra
		val uint
	when Empty

export main() -> c_int
	.a = Union.Extra(2, 4)
	a.common = 4
	a = Union.Empty(3)

	match a as b
		when Union.Extra
			b.val
			b.common
			puts("Extra!")
		when Union[bool].Empty
			b.common
			puts("Empty!")
		else
			puts("Else!")

	return 0