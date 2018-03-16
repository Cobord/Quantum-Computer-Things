{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

{- --didn't end up needing these so commented out
data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- make a vector of length n filled with same things
replicate :: SNat n -> a -> Vector a n
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a

--takes a natural number n and a list of integers and puts them into a vector of length n
-- default to -1 if the list is too short
fromList:: SNat n -> [Int] -> Vector Int n
fromList SZ _ = Nil
fromList (SS n) [] = replicate (SS n) (-1)
fromList (SS n) (x:xs) = x :- (fromList n xs)
-}

data ErrorCorrectionFlags = Steane | BitFlip | SignFlip | Shor deriving (Read,Eq,Show)

data ECCScheme = None | S1 ECCScheme | S2 ECCScheme | S3 ECCScheme | S4 ECCScheme deriving (Show)

{-Internal indices stores the index for the qubit with the error correction scheme. This is done by
If there is no error correction then it is Lgcl a to say it's the a'th logical qubit.
If applied a Steane code then see Int1 a (rest) which means it is a is the index within the Steane code and rest is the index before
Same with the rest of the ErrorCorrectionFlags. Compose them as long as desired
-}
data InternalIndices a n where
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

-- occupation
data ECCSchemeCopy n where
  NoneCopy :: ECCSchemeCopy None
  S1Copy :: ECCSchemeCopy n -> ECCSchemeCopy (S1 n)
  S2Copy :: ECCSchemeCopy n -> ECCSchemeCopy (S2 n)
  S3Copy :: ECCSchemeCopy n -> ECCSchemeCopy (S3 n)
  S4Copy :: ECCSchemeCopy n -> ECCSchemeCopy (S4 n)

-- puts the same x in all the internal indices for each stage of ECCSchemeCopy n
myreplicate :: ECCSchemeCopy n -> a -> InternalIndices a n
myreplicate NoneCopy x = (Lgcl x)
myreplicate (S1Copy n) x = Int1 x (myreplicate n x)
myreplicate (S2Copy n) x = Int2 x (myreplicate n x)
myreplicate (S3Copy n) x = Int3 x (myreplicate n x)
myreplicate (S4Copy n) x = Int4 x (myreplicate n x)

codeLengths::ErrorCorrectionFlags -> Int
codeLengths Steane = 7
codeLengths BitFlip = 3
codeLengths SignFlip = 3
codeLengths Shor = 9

myMod:: Int -> Int -> Int
myMod x y = x `mod` y

-- takes a vector of integers of length n for the internal indices at each stage and the flags at each stage
-- reduces them all to make sure they are within the appropriate bounds
-- for example if we see (4,5,6) with flags (SignFlip,Shor,Steane) we would know this took one logical qubit looked at the 6th upon applying a Steane code
-- then the 5th of a Shor code then the 1st=4th of a SignFlip code
reduceGateIndices:: (Vector Int n) -> (Vector ErrorCorrectionFlags n) -> [Int]
reduceGateIndices x y = zipWith myMod (toList x) allCodeLengths where allCodeLengths=map codeLengths $ toList y

-- similar to above but with (InternalIndices Int n) format instead of vector and list format
reduceGateIndices2:: InternalIndices Int n -> InternalIndices Int n
reduceGateIndices2 (Lgcl x) = Lgcl x
reduceGateIndices2 (Int1 x xs) = Int1 (x `mod` (codeLengths Steane)) (reduceGateIndices2 xs)
reduceGateIndices2 (Int2 x xs) = Int2 (x `mod` (codeLengths BitFlip)) (reduceGateIndices2 xs)
reduceGateIndices2 (Int3 x xs) = Int3 (x `mod` (codeLengths SignFlip)) (reduceGateIndices2 xs)
reduceGateIndices2 (Int4 x xs) = Int4 (x `mod` (codeLengths Shor)) (reduceGateIndices2 xs)

data GateNames = PauliX1 | PauliY1 | PauliZ1 | Hadamard1 | QuarterPhase1 | SqrtSwap2 | CNOT2 | (CRTheta n) deriving (Read, Show, Eq)

-- how many qubits does this kind of gate operate on
arity::GateNames -> Int
arity PauliX1 = 1
arity PauliY1 = 1
arity PauliZ1 = 1
arity Hadamard1 = 1
arity QuarterPhase1 = 1
arity SqrtSwap2 = 2
arity CNOT2 = 2

-- A gate is given as a name and a list of involved qubits
data GateData n = GateData{name::GateNames, myinvolvedQubits::[InternalIndices Int n]} deriving (Eq,Show)

-- how many qubits does this actual gate operate on
arity2:: GateData n -> Int
arity2 x = arity (name x)

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

--check that the arity matches the number of qubits operated on. Then makes sure that the two are distinct if supposed to operate on 2
checkValidGate::GateData n -> Bool
checkValidGate x = (((( length qubits)) == (arity2 x)) && (allDifferent qubits)) where qubits = ( myinvolvedQubits x)

-- assuming g1 and g2 are valid gates check if they commute in the simplest way which is they have disjoint supports
definitelyCommute:: GateData n -> GateData n -> Bool
definitelyCommute x y
    | inCommon==[] = True
	| otherwise = False
    where inCommon=(myinvolvedQubits x) 'intersect' (myinvolvedQubits y)


{-
-- puts a gate with same name in the 0th index of the corresponding error correction flag. This corresponds to the case when just adjoined ancillas
-- for the k-1 in each batch but not did an error correction. The gates just operate on the 0th indices.
implementECC1Helper :: GateData n -> GateData (S1 n)
implementECC1Helper x = GateData(name=(name x),myinvolvedQubits = Int1 0 (myinvolvedQubits x))
implementECC2Helper :: GateData n -> GateData (S2 n)
implementECC2Helper x = GateData(name=(name x),myinvolvedQubits = Int2 0 (myinvolvedQubits x))
implementECC3Helper :: GateData n -> GateData (S3 n)
implementECC3Helper x = GateData(name=(name x),myinvolvedQubits = Int3 0 (myinvolvedQubits x))
implementECC4Helper :: GateData n -> GateData (S4 n)
implementECC4Helper x = GateData(name=(name x),myinvolvedQubits = Int4 0 (myinvolvedQubits x))

--Take a gate with internal indices in a ECCScheme n and produces the corresponding circuit that implements the same gate but after an additional
-- error correction step. This is done by conjugation by encoding/decoding
-- Caution: the order is left to right of application
implementECC1:: (GateData n) -> [(GateData (S1 n)])
implementECC1 GateData(name=y,myinvolvedQubits=[x]) = [?decoding circuit]:(implementECC1Helper y):[?encoding circuit]
implementECC1Mapper:: [(GateData n)] -> [(GateData (S1 n)])
implementECC1Mapper x = concatMap (implementECC1) x
implementECC2:: (GateData n) -> [(GateData (S2 n)])
implementECC2 GateData(name=y,myinvolvedQubits=[x]) = [?decoding circuit]:(implementECC2Helper y):[?encoding circuit]
implementECC2Mapper:: [(GateData n)] -> [(GateData (S2 n)])
implementECC2Mapper x = concatMap (implementECC2) x
implementECC3:: (GateData n) -> [(GateData (S3 n)])
implementECC3 GateData(name=y,myinvolvedQubits=[x]) = [?decoding circuit]:(implementECC3Helper y):[?encoding circuit]
implementECC3Mapper:: [(GateData n)] -> [(GateData (S3 n)])
implementECC3Mapper x = concatMap (implementECC3) x
implementECC4:: (GateData n) -> [(GateData (S4 n)])
implementECC4 GateData(name=y,myinvolvedQubits=[x]) = [?decoding circuit]:(implementECC4Helper y):[?encoding circuit]
implementECC4Mapper:: [(GateData n)] -> [(GateData (S4 n)])
implementECC4Mapper x = concatMap (implementECC4) x
-}

--Common logical circuits applyed with None as ECCScheme, if want with error correction then apply the corresponding implementECC Mapper in succession
{-
-- quantumFourierTransform N gives the circuit that implements said QFT on N logical qubits indexed as 0 through N-1
-- works inductively bc given as related to (quantumFourierTransform N-1) before any potential simplifications
quantumFourierTransform :: Int -> [GateData None]
quantumFourierTransform 0 = []
quantumFourierTransform 1 = GateData(name=Hadamard1,myinvolvedQubits=[Lgcl 0])
quantumFourierTransform N = GateData(name=Hadamard1,myinvolvedQubits=[Lgcl N-1]):( all the controlled rotations on N-1 and rest): (quantumFourierTransform (N-1))
-- Grover's search takes the black boxed circuit and and integer m which should be O(\sqrt{N}) and produce the grovers Search algorithm circuit
groversSearch :: [GateData None] -> Int -> [GateData None]
-}