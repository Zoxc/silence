struct String
	data *char
	size uint
	
	action create :
		size <- 0,
		data <- force_cast malloc(0)
	
	action copy
		.new = malloc(size)
		memcpy(new, force_cast data, size)
		data = force_cast new

	action destroy
		free(force_cast data)

	shared from_data(data *char, size)
		.new = malloc(size)
		memcpy(new, force_cast data, size)
		return String(force_cast new, size)

	c_str(f)
		.z char = 0
		.str = self ~ String.from_data(&z, 1)
		return f(str.data)

	each(f)
		times(size, |i| f(*ptr_idx(data, i)))

instance StringLiteral(String)
	create(data *char, size)
		return String.from_data(data, size)

instance Joinable(String)
	join(lhs String, rhs String)
		.new_size = lhs.size + rhs.size
		.new = malloc(new_size)
		memcpy(new, force_cast lhs.data, lhs.size)
		.rhs_pos uint = force_cast new + lhs.size
		memcpy(force_cast rhs_pos, force_cast rhs.data, rhs.size)
		return String(force_cast new, new_size)
