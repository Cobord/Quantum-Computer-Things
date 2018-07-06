-- https://arxiv.org/pdf/1210.3018.pdf

{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Maybe
import Data.List (tails)

data Nat = Z | S Nat deriving (Show)

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

convert :: Int -> Nat
convert x
          | x <= 0 = Z
          | otherwise = S (convert (x-1))

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

instance Show (SNat n) where
  show SZ = "SZ"
  show (SS x) = "SS " ++ (show x)

-- make a vector of length n filled with same things
myReplicate :: SNat n -> a -> Vector a n
myReplicate SZ     _ = Nil
myReplicate (SS n) a = a :- myReplicate n a

--takes a natural number n and a list of integers and puts them into a vector of length n
-- default to defaultVal if the list is too short
fromList:: SNat n -> [a] -> a -> Vector a n
fromList SZ _ _ = Nil
fromList (SS n) [] defaultVal = myReplicate (SS n) (defaultVal)
fromList (SS n) (x:xs) defaultVal = x :- (fromList n xs defaultVal)

sumNat2 :: SNat n -> SNat m -> SNat (n :+ m)
sumNat2 SZ x = x
sumNat2 (SS x) y = SS (sumNat2 x y)

data Ordinal (n :: Nat) where
    OZ :: Ordinal (S n)
    OS :: Ordinal n -> Ordinal (S n)

myEq :: Ordinal n -> Ordinal n -> Bool
myEq OZ OZ = True
myEq (OS a) (OS b) = (a==b)
myEq _ _ = False

instance Eq (Ordinal n) where
    x==y = (myEq x y)

asInteger :: Ordinal n -> Int
asInteger OZ = 0
asInteger (OS x) = 1+ (asInteger x)
  
instance Show (Ordinal n) where
  show x = show $ asInteger x

sIndex :: Ordinal n -> Vector a n -> a
sIndex OZ     (x :- _)  = x
sIndex (OS n) (_ :- xs) = sIndex n xs

-- the first is the outcomes, the second is the measurements
-- n is the number of parties
data Event n m d = Event{outcomes::(Vector (Ordinal d) n),measurements::Vector (Ordinal m) n}

extractPair :: Event n m d -> (Vector (Ordinal d) n,Vector (Ordinal m) n)
extractPair e1 = (outcomes e1,measurements e1)

locallyOrthogonal0 :: (Vector (Ordinal d) n,Vector (Ordinal m) n) -> (Vector (Ordinal d) n,Vector (Ordinal m) n) -> Bool
locallyOrthogonal0 (Nil,_) (_,_) = False
locallyOrthogonal0 (x:-xs,y :- ys) (z:-zs,w :- ws)
                                                 | ((y==w) && (not (x == z))) = True
                                                 | otherwise = locallyOrthogonal0 (xs,ys) (zs,ws)
locallyOrthogonal :: Event n m d -> Event n m d -> Bool
locallyOrthogonal e1 e2 = locallyOrthogonal0 (extractPair e1) (extractPair e2)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

locallyOrthogonal2 :: [Event n m d] -> Bool
locallyOrthogonal2 xs = and [locallyOrthogonal fst snd| (fst,snd) <- pairs xs]

data ProbDist n m d = ProbDist{probFunc::(Event n m d) -> Double}

orthogonal3 :: [Event n m d] -> ProbDist n m d -> Bool
orthogonal3 es prob = (locallyOrthogonal2 es) && (sum [(probFunc prob) e | e <- es] < 1)