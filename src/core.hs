use "core/import"
use "core/string"
use "core/array"

instance[T Indexable] Callable T
	alias Result = Indexable[T].Result
	alias Args = Indexable[T].Index

	apply(args Args)
		return *Indexable(self).ref(args)
