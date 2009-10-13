degree p = length normalize p



-- normalize function: removes leading zeroes

normalize [] = []
normalize 0::x = normalize x
normalize x = x


pld _ []	= ([1/0], [1/0])
pld [] _	= ([],[])
pld n::ns d::ds
	| length ns < length ds	= ([],n::ns)
	| otherwise				=	let	q0	= n/d
								 	r0	= detract n::ns q0 d::ds
									(q1, r1)	= pld r0 d::ds
								in	(q0::q1, r1)
-- TODO
-- add a polynomial_long_division that calls pld sanitized on input (normalize)
