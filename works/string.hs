struct String
	data *char
	size uint

instance StringLiteral String
	create(data *char, size uint)
		return String(data, size)

instance Joinable String
	join(lhs String, rhs String) -> String
		.new_size = lhs.size + rhs.size
		.new = malloc(new_size + 1)
		memcpy(force_cast new, force_cast lhs.data, lhs.size)
		.rhs_pos uint = force_cast new + lhs.size
		memcpy(force_cast rhs_pos, force_cast rhs.data, rhs.size)
		*force_cast[*char](rhs_pos + rhs.size) = 0
		return String(new, new_size)

test()
	.var = "Hello"
	return var ~ " world!"
	
import malloc(size uint) -> *()
import memcpy(dst *(), src *(), len uint) -> *()
import puts(str *char)

export main()
	.var String = test()
	puts(var.data)