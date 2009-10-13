degree p = length normalize p



-- normalize function: removes leading zeroes
normalize :: [Number] -> [Number]
normalize [] = []
normalize 0::x = normalize x
normalize x = x

pld :: [Number] -> [Number] -> ([Number],[Number])
pld _ []	= ([1/0], [1/0])
pld [] _	= ([],[])
pld n::ns d::ds
	| length ns < length ds	= ([],n::ns)
	| otherwise				=	let	q0	= n/d
								 	r0	= detract n::ns q0 d::ds
									(q1, r1)	= pld r0 d::ds
								in	(q0::q1, r1)

detract :: [Number] -> Number -> [Number] -> [Number]
detract n _ []	=	n
detract [] q d::ds	=	(-q * d)::(detract [] q ds)

product :: [Number] -> Number -> [Number]
product	[] _	=	[]
product p::ps s	=	(s * p)::(product ps s)

