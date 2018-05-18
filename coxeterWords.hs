import qualified Data.Graph as Graph
import Data.Maybe

data CoxGens = S12 | S23 | S34 | S45 deriving (Show,Read,Eq,Enum,Bounded)

allGens = [(minBound::CoxGens) ..]

--parseGen :: Parser CoxGens
--parseGen = fmap read . foldr1 (<|>) $ map (try . string . show) [ (minBound :: CoxGens) ..] 

-- take a word and give the segments for repeated letters
-- for example, ABCBA will give [(0,4),(1,3)]
toTest :: [CoxGens] -> CoxGens -> [(Int,Int)]
toTest xs z = [(i,j) | (i,j) <- zip is (tail is)] where is = [i | (i, j) <- zip [0..] xs, j==z]

-- segment of word between j1 and j2 inclusive
splitWord :: [CoxGens] -> (Int,Int) -> [CoxGens]
splitWord xs (j1,j2) = [x | (i,x) <- zip [0..] xs , i>=j1 , i<=j2 ]

-- neighbors in the coxeter graph. Uses Maybe to account for the fact that might be infinite
neighbors :: CoxGens -> (CoxGens -> CoxGens -> Maybe Int) -> [CoxGens]
neighbors x mij = [y | y <- allGens , (isNothing $ (mij x y)) || ((fromJust $ (mij x y)) > 2)]

-- given a segment xs and the letter to be tested z, check if xs has all the neighbors of z
containsAllNeighbors :: [CoxGens] -> CoxGens -> (CoxGens -> CoxGens -> Maybe Int) -> Bool
containsAllNeighbors xs z mij = and [elem y xs | y <- neighbors z mij]

--reduciblePiece :: [CoxGens] -> CoxGens -> (CoxGens -> CoxGens -> Int)

-- take a word in the coxeter generators and the function encoding m_ij and give
-- the indices of a potentially reducible subword
interveningNeighbors :: [CoxGens] -> (CoxGens -> CoxGens -> Maybe Int) -> [(Int,Int)]
interveningNeighbors xs mij = concat [[ij | ij <- toTest xs z, not $ containsAllNeighbors (splitWord xs ij) z mij] | z <- allGens]

-- each of these will fail to have all their neighbors
offendingWords :: [CoxGens] -> (CoxGens -> CoxGens -> Maybe Int) -> [[CoxGens]]
offendingWords xs mij = [splitWord xs ij | ij <- interveningNeighbors xs mij]