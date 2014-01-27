use "core/import"
use "core/string"
use "core/array"
use "core/io"
use "core/file"

instance Binary(bool)
	not()
		return if self == true
			false
		else
			true

	shared and(lhs bool, rhs)
		return if lhs == true
			if rhs == true
				true
			else
				false
		else
			false

	shared or(lhs bool, rhs)
		return if lhs == true
			true
		else
			if rhs == true
				true
			else
				false

	shared xor(lhs bool, rhs)
		return !(lhs == rhs)

instance(T) Reference(*T)
	alias Type = T

	get() -> *T
		return self

class Show(T)
	show() -> String

instance Show(uint)
	show()
		.buf Table(20) = undef()
		.size = C.sprintf(&buf(0), "%lu", self)
		return String.from_data(&buf(0), force_cast size)

show(t)
	return Show(t).show()

instance StringLiteral(*char)
	create(data *char, size uint)
		return data

instance StringLiteral(char)
	create(data *char, size uint)
		assert -> size == 1
		return *data

instance(T) Eq(*T)
	equal(lhs *T, rhs *T)
		return force_cast[uint] lhs == force_cast rhs

assert(b)
	if !b()
		C.abort()

struct Option(T)
	when Some
		val T
	when Nil

int_ptr[P](p *P)
	return force_cast[uint](p)

nil_ptr(p)
	return int_ptr(p) == 0

times_impl(i, max, f)
	if i > 0
		f(max - i)
		times_impl(i - 1, max, f)

times(i, f)
	times_impl(i, i, f)

for_range(a, b, f)
	.i uint = force_cast(b - a) + 1
	times(i, |i| f(a + force_cast i))

ptr_idx[T](ptr *T, idx uint) -> *T
	return force_cast(force_cast ptr + idx * size_of[T]())

instance(T Indexable) Callable(T)
	alias Result = Indexable(T).Result
	alias Args = Indexable(T).Index

	apply(args Args)
		return *Indexable(self).ref(args)
