
a(v Callable, args Tuple)
	return v.apply(args)

import printf(str *char)

export main()
	.args Cell[*char ,Unit]
	
	a(printf, args)
	
	return 0
