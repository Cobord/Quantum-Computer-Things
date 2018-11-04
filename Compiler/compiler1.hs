--AllowedGateNames for the 1+2 qubit operations that are implemented in the Hardware
--GateNames for the bigger set that makes programming easier. For example, suppose
--		the sqrt[swap] is implemented but not the CNOT, then rather than write the
--		full expression for CNOT in terms of sqrt[swap],RY,RZ,and Z, write the CNOT
--		then let the compiler substitute the full expression in later

--This set is all the possible operations you might want to use. This is the data type that
--   doesn't care if they are implemented or not so might as well put all of them.
--   only tedious pain in the ass is you have to give the transformation rule into allowed for each of them.
data GateNames1q = PauliX1 | PauliY1 | PauliZ1 deriving (Read, Show, Eq)
data GateNames2q = SqrtSwap2 | CNOT2 deriving (Read, Show, Eq)
data GateName = PauliX | PauliY | PauliZ | SqrtSwap | CNOT
toGateName1::GateNames1q -> GateName
toGateName1 PauliX1 = PauliX
toGateName1 PauliY1 = PauliY
toGateName1 PauliZ1 = PauliZ
toGateName2::GateNames2q -> GateName
toGateName2 SqrtSwap2 = SqrtSwap
toGateName2 CNOT2 = CNOT

data AllowedGateNames1q = APauliX1 | APauliY1 | APauliZ1 deriving (Read, Show,Eq)
data AllowedGateNames2q = ASqrtSwap2 deriving (Read, Show, Eq)
toGateName1A::AllowedGateNames1q -> GateNames1q
toGateName1A APauliX1 = PauliX1
toGateName1A APauliY1 = PauliY1
toGateName1A APauliZ1 = PauliZ1
toGateName2A::AllowedGateNames2q -> GateNames2q
toGateName2A ASqrtSwap2 = SqrtSwap2

-- Number of logical qubits
maximumLogical :: Int
maximumLogical = 8
newtype QubitIndices = MakeLogicalIndex Int
toQubitIndex :: Int -> QubitIndices
toQubitIndex x = MakeLogicalIndex (x `mod` maximumLogical)
fromQubitIndex :: QubitIndices -> Int
fromQubitIndex (MakeLogicalIndex i) = i
instance Show QubitIndices where
    show x = show ((fromQubitIndex x) `mod` maximumLogical)
instance Eq QubitIndices where
    x == y = (((fromQubitIndex x) - (fromQubitIndex y)) `rem` maximumLogical == 0)

-- The Steane code puts every qubit into 7 qubits
-- if you don't want to put error correction then set all the ECCIndex's to 1
data ErrorCorrectionFlags = None | Steane | BitFlip | SignFlip | Shor deriving (Read,Eq,Show)
internalQubits::ErrorCorrectionFlags -> Int
internalQubits None = 1
internalQubits Steane = 7
internalQubits BitFlip = 3
internalQubits SignFlip = 3
internalQubits Shor = 9
internalQubits2::[ErrorCorrectionFlags] -> Int
internalQubits2 xs = foldl (\acc x -> acc*(internalQubits x)) 1 xs
eccFlag :: [ErrorCorrectionFlags]
eccFlag = [Steane]
maximumInternal::Int
maximumInternal = internalQubits2 eccFlag
newtype ECCIndex = MakeECCIndex Int
toECCQubitIndex :: Int -> ECCIndex
toECCQubitIndex x = MakeECCIndex (x `mod` maximumInternal)
fromECCQubitIndex :: ECCIndex -> Int
fromECCQubitIndex (MakeECCIndex i) = i
instance Show ECCIndex where show x = show ((fromECCQubitIndex x) `mod` maximumInternal)
instance Eq ECCIndex where x==y = (((fromECCQubitIndex x)-(fromECCQubitIndex y)) `rem` maximumInternal == 0)
-- If don't want error correction
--eccFlag = [None]
defaultECCIndex::ECCIndex
defaultECCIndex = toECCQubitIndex 0
data QubitIndicesECC = QubitIndicesECC{ qubitIndexECC:: QubitIndices, internalIndex:: ECCIndex} deriving (Eq)
instance Show QubitIndicesECC where
    show x = show (qubitIndexECC x) ++ "I" ++ show (internalIndex x)
-- a qubit in the error correcting code is shown and read as 14I3 for the 3rd internal index of the 14th logical qubit
-- they are written as QubitIndicesECC{ qubitIndexECC=toQubitIndex 1, internalIndex=toECCQubitIndex 3} in the terminal