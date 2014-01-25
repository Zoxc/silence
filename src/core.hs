use "core/import"
use "core/string"
use "core/array"
use "core/io"

times_impl(i, max, f)
	if i > 0
		f(max - i)
		times_impl(i - 1, max, f)

times(i, f)
	times_impl(i, i, f)

ptr_idx[T](ptr *T, idx uint) -> *T
	return force_cast(force_cast ptr + idx * size_of[T]())

instance[T Indexable] Callable T
	alias Result = Indexable[T].Result
	alias Args = Indexable[T].Index

	apply(args Args)
		return *Indexable(self).ref(args)
