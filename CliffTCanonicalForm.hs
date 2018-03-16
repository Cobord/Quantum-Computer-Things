--https://arxiv.org/pdf/1803.05047.pdf

data GateNames = H | S | T deriving (Read,Eq,Show)

data LPart = [] | [H] | [H,S] | [H,S,S]
data LPrimePart = [H] | [H,S] | [H,S,S]
data MPart = [] | [H,H]

tCount:: [GateNames] -> Int
tCount = length . filter (==T)

-- read backwards so H,S,T,S corresponds to the group element STSH
canonicalForm:: [GateNames] -> [GateNames]
canonicalForm [] = []
canonicalForm x:xs =
					| tCount (canonicalFormRest)==0 = 
					| tCount (canonicalFormRest)==1 = 
					| tCount (canonicalFormRest)==2 = 
					where canonicalFormRest=canonicalForm xs
