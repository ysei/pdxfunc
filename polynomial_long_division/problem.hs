degree p = length normalize p



-- normalize function: removes trailing zeroes
normalize [] = []

normalize 0::[] = []

normalize x::[] = x::[]
