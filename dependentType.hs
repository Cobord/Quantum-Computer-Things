{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (tail, head, replicate)
data Nat = Z | S Nat deriving (Show)

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

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

replicate :: SNat n -> a -> Vector a n
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a

fromList:: SNat n -> [Int] -> Vector Int n
fromList SZ _ = Nil
fromList (SS n) [] = replicate (SS n) (-1)
fromList (SS n) (x:xs) = x :- (fromList n xs)

data ErrorCorrectionFlags = Steane | BitFlip | SignFlip | Shor deriving (Read,Eq,Show)

data ECCScheme = None | S1 ECCScheme | S2 ECCScheme | S3 ECCScheme | S4 ECCScheme deriving (Show)

data InternalIndices a n where
--  Nil2  :: InternalIndices a None
  Lgcl  :: a -> InternalIndices a None
  Int1 :: a -> InternalIndices a n -> InternalIndices a (S1 n)
  Int2 :: a -> InternalIndices a n -> InternalIndices a (S2 n)
  Int3 :: a -> InternalIndices a n -> InternalIndices a (S3 n)
  Int4 :: a -> InternalIndices a n -> InternalIndices a (S4 n)

deriving instance Eq a => Eq (InternalIndices a n)

toList2 :: InternalIndices a n -> [a]
--toList2 Nil2 = []
toList2 (Lgcl x) = [x]
toList2 (Int1 x xs) = x : toList2 xs
toList2 (Int2 x xs) = x : toList2 xs
toList2 (Int3 x xs) = x : toList2 xs
toList2 (Int4 x xs) = x : toList2 xs

instance Show a => Show (InternalIndices a n) where
  showsPrec d = showsPrec d . toList2

codeLengths::ErrorCorrectionFlags -> Int
codeLengths Steane = 7
codeLengths BitFlip = 3
codeLengths SignFlip = 3
codeLengths Shor = 9

myMod:: Int -> Int -> Int
myMod x y = x `mod` y

reduceGateIndices:: (Vector Int n) -> (Vector ErrorCorrectionFlags n) -> [Int]
reduceGateIndices x y = zipWith myMod (toList x) allCodeLengths where allCodeLengths=map codeLengths $ toList y

reduceGateIndices2:: InternalIndices Int n -> InternalIndices Int n
--reduceGateIndices2 Nil2 = Nil2
reduceGateIndices2 (Lgcl x) = Lgcl x
reduceGateIndices2 (Int1 x xs) = Int1 (x `mod` (codeLengths Steane)) xs
reduceGateIndices2 (Int2 x xs) = Int2 (x `mod` (codeLengths BitFlip)) xs
reduceGateIndices2 (Int3 x xs) = Int3 (x `mod` (codeLengths SignFlip)) xs
reduceGateIndices2 (Int4 x xs) = Int4 (x `mod` (codeLengths Shor)) xs

data GateData n = GateData{name::String, myinvolvedQubit::(InternalIndices Int n)} deriving (Eq,Show)