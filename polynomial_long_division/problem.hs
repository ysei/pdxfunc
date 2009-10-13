degree p = length normalize p



-- normalize function: removes leading zeroes

normalize [] = []
normalize 0::x = normalize x
normalize x = x


pld N [] = ([1/0], [1/0])
pld N D
	| length N < length D = (0,N)
	| length N > length D = pld 

-- TODO
-- add a polynomial_long_division that calls pld sanitized on input (normalize)
