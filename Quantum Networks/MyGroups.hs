module MyGroups
( listProduct,
  FiniteGroup(..),
  CyclicGroup2(..),
  CyclicGroup5(..),
  DihedralGroup5(..)
) where

--import Math.Combinat.Permutations

class (Eq a) => FiniteGroup a where
    mult :: a -> a -> a
    inv :: a -> a
    identity :: a
--    ((mult x identity) == x) = True
--    ((mult identity x) == x) = True
--    ((inv identity) == identity) = True
--    ((mult x (inv x)) == identity) = True
--    ((mult (inv x) x) == identity) = True
--    ((inv $ mult x y) == (mult (inv y) (inv x))) = True

--g_1 \cdots g_n in the group are given and it multiplies them in that order
listProduct :: (FiniteGroup a) => [a] -> a  
listProduct xs = foldl (\acc x -> mult acc x) identity xs

data CyclicGroup2 = Id | Flip
                    deriving (Eq,Show,Read)

instance FiniteGroup CyclicGroup2 where
    mult Id Id = Id
    mult Flip Id = Flip
    mult Id Flip = Flip
    mult Flip Flip = Id
    inv Id = Id
    inv Flip = Flip
    identity = Id

--Don't want to deriving enum because want the group in one file and the Verhoeff part in another file.
--Verhoeff file controls the choice of permutation T and the translation from the alphabet to group elements
instance Enum CyclicGroup2 where
    toEnum 0 = Id
    toEnum 1 = Flip
    fromEnum Id = 0
    fromEnum Flip = 1

data CyclicGroup5 = RotZero | RotOne | RotTwo | RotThree | RotFour
                    deriving (Eq,Show,Read,Enum)

--Use the isomorphism of cyclic group of order n with addition modulo n by using the enum
--To change the group to a different order add more or less Rots and modify the 5 below to n
instance FiniteGroup CyclicGroup5 where
    mult a b = toEnum (((fromEnum a) + (fromEnum b)) `mod` 5)
    inv a = toEnum (( 5 - (fromEnum a)) `mod` 5)
    identity = RotZero

-- stored as r*f or r*t where r is the rotationPrefix
-- so r_1*f_1 r_2*f_2 = (r_1 r_2^(\mp f_1)) f_1 f_2
-- (r_1 f_1)^(-1) = f_1 r_1^(-1) = r_1^(\pm) f_1
--Other dihedral groups can be made by modifying the cyclic group
--everything else follows mutatis mutandis
data DihedralGroup5 = DihedralGroup5 { rotationPrefix::CyclicGroup5
                                       , flipped :: Bool
                                      } deriving (Eq, Read)

instance Show DihedralGroup5 where
    show a
         | (flipped a) = (show (rotationPrefix a)) ++ "*Flip"
         | (flipped a == False) = (show (rotationPrefix a))
         | otherwise = "Error"

xor:: Bool -> Bool -> Bool
xor a b = a /= b

--multiply or divide depending on the boolean argument
plusMinus:: CyclicGroup5 -> CyclicGroup5 -> Bool -> CyclicGroup5
plusMinus x y False = mult x y
plusMinus x y True = mult x (inv y)

invertPlusMinus:: CyclicGroup5 -> Bool -> DihedralGroup5
invertPlusMinus r True = DihedralGroup5 {rotationPrefix = r,flipped=True}
invertPlusMinus r False = DihedralGroup5 {rotationPrefix = inv r,flipped=False}

--realize the dihedral group as a group with it's multiplication and inversion operation
instance FiniteGroup DihedralGroup5 where
    identity = DihedralGroup5 {rotationPrefix=RotZero,flipped=False}
    mult a b = DihedralGroup5 {rotationPrefix=(plusMinus (rotationPrefix a) (rotationPrefix b) (flipped a)),flipped = (xor (flipped a) (flipped b))}
    inv a = invertPlusMinus (rotationPrefix a) (flipped a)