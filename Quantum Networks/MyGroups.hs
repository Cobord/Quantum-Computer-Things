{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MyGroups
( listProduct,
  FiniteGroup(..),
  CyclicGroup2(..),
  CyclicGroup5(..),
  DihedralGroup5(..)
) where

--import Math.Combinat.Permutations

data Nat = Z | S Nat

convertToNat :: Int -> Nat
convertToNat x
          | x <= 0 = Z
          | otherwise = S (convertToNat (x-1))

numericValNat :: Nat -> Int
numericValNat Z = 0
numericValNat (S x) = 1+(numericValNat x)

type family Plus (n :: Nat) (m :: Nat) :: Nat where
          Plus Z m = m
          Plus (S n) m = S (Plus n m)

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

instance Show (SNat n) where
  show SZ = "SZ"
  show (SS x) = "SS " ++ (show x)

numericVal :: SNat n -> Int
numericVal SZ = 0
numericVal (SS x) = 1 + numericVal x

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

class SNatural a
instance SNatural (SNat n)

class (Eq a,SNatural b) => FiniteGroupParam a b where
    multParam :: (a,b) -> (a,b) -> (a,b)
    invParam :: (a,b) -> (a,b)
    identityParam :: b -> (a,b)

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

data (CyclicGroup n) = CyclicGroup{underlyingVal::Int,myN::SNat n}

instance Eq (CyclicGroup n) where
    x==y = ((underlyingVal x) - (underlyingVal y) `mod` (numericVal (myN x)))==0

instance Show (CyclicGroup n) where
    show a = (show $ underlyingVal a) ++ " mod " ++ (show $ myN a)

data (Ordinal n) where
  OZ :: Ordinal (S n)
  OS :: Ordinal n -> Ordinal (S n)

asInteger :: Ordinal n -> Int
asInteger OZ = 0
asInteger (OS x) = 1+ (asInteger x)
  
instance Show (Ordinal n) where
  show x = show $ asInteger x

data (Permutation n) = Permutation{myNum :: SNat n,transpositions::[(Ordinal n,Ordinal n)]}

whereGoesHelper :: (Ordinal n,Ordinal n) -> Int -> Int
whereGoesHelper (y1,y2) x
                          | (x == asInteger y1) = asInteger y2
                          | (x == asInteger y2) = asInteger y1
                          | otherwise = x
whereGoes :: [(Ordinal n,Ordinal n)] -> Int -> Int
whereGoes [] x = x
whereGoes (y:ys) x = whereGoes ys (whereGoesHelper y x)

instance Eq (Permutation n) where
    x1==x2 = (n1==(numericVal $ myNum x2)) && and [ (whereGoes (transpositions x1) x) == (whereGoes (transpositions x2) x) | x <- [0..(-1+n1)]] where n1=numericVal $ myNum x1

instance FiniteGroupParam (Permutation n) (SNat n) where
    multParam (x1,x2) (y1,y2) = (Permutation{myNum=x2,transpositions=(transpositions x1)++(transpositions y1)},x2)
    invParam (x1,x2) = (Permutation{myNum=x2,transpositions=reverse (transpositions x1)},x2)
    identityParam x2 = (Permutation{myNum=x2,transpositions=[]},x2)