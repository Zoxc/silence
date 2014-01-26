struct File
	shared read(path String)
		.c_path = path.c_str()
		.f = C.fopen(c_path.data, "r")

		if nil_ptr(f)
			return Option.Nil()

		.result String

		.r = ->
			.c = C.fgetc(f)
			if c != -1
				.u char = force_cast c
				result ~= String(&u, 1)
				r()

		r()

		C.fclose(f)

		return Option.Some(result)
