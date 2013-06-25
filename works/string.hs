import malloc(size uint) -> *()
import free(mem *())
import memcpy(dst *(), src *(), len uint)
import puts(str *char)

struct String
	data *char
	size uint
	
	action create
		size = 0
	
	action copy
		.new = malloc(size + 1)
		memcpy(new, force_cast data, size + 1)
		data = force_cast new

	action destroy
		free(force_cast data)

instance StringLiteral String
	create(data *char, size)
		.new = malloc(size + 1)
		memcpy(new, force_cast data, size)
		*force_cast[*char](force_cast new + size) = 0
		return String(force_cast new, size)

instance Joinable String
	join(lhs String, rhs String)
		.new_size = lhs.size + rhs.size
		.new = malloc(new_size + 1)
		memcpy(new, force_cast lhs.data, lhs.size)
		.rhs_pos uint = force_cast new + lhs.size
		memcpy(force_cast rhs_pos, force_cast rhs.data, rhs.size)
		*force_cast[*char](rhs_pos + rhs.size) = 0
		return String(force_cast new, new_size)

test()
	.var = "Hello"
	return var ~ " world!"
	
export main() -> int
	.var2 String
	.var String = test()
	puts(var.data)
	return 0