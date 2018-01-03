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

--Gates stores a name and the indices of which qubits it is operating on

data AllowedGates1q = AllowedGates1q{ allowedname1q::AllowedGateNames1q, allowedinvolvedQubit::QubitIndices} deriving (Eq)
instance Show AllowedGates1q where
        show x = show (allowedname1q x) ++ " on qubit " ++ show (allowedinvolvedQubit x)
instance Read AllowedGates1q where
        read x = AllowedGate1q{ allowedname1q=read (head y), allowedinvolvedQubit=read (last y)} where y = (words x)
data AllowedGates2q = AllowedGates2q{ allowedname2q::AllowedGateNames2q, allowedinvolvedQubits::(QubitIndices,QubitIndices)} deriving (Eq)
instance Show AllowedGates2q where
        show x = show (allowedname2q x) ++ " on qubits " ++ show (allowedinvolvedQubits x)
instance Read AllowedGates2q where
        read x = AllowedGate2q{ allowedname2q=read (head y), allowedinvolvedQubits=read (last y)} where y = (words x)
default1qAllowedGate = AllowedGates1q{allowedname1q=PauliX,allowedinvolvedQubit=1}
default2qAllowedGate = AllowedGates2q{allowedname2q=SqrtSwap,allowedinvolvedQubits=(1,2)}
data AllowedGates = Either AllowedGates1q AllowedGates2q deriving (Eq)
instance Show AllowedGates where
        show x
                | isLeft x = show (fromLeft default1qAllowedGate x)
                | otherwise = show (fromRight default2qAllowedGate x)
instance Read AllowedGates where
        read x
                | y =="1q" = Left read x
                | y =="2q" = Right read x
                where y = head tail words x
-- input an allowed gate as "APauliX1 1q on qubit 4 or ASqrtSwap2 2q on qubits (3,4)"

data AllowedGates1qECC = AllowedGates1qECC{ allowedname1qECC::AllowedGateNames1q, allowedinvolvedQubitECC::QubitIndicesECC} deriving (Eq)
instance Show AllowedGates1qECC where
        show x = show (allowedname1qECC x) ++ " on qubit " ++ show (allowedinvolvedQubitECC x)
instance Read AllowedGates1qECC where
        read x = AllowedGate1qECC{ allowedname1qECC=read (head y), allowedinvolvedQubitECC=read (last y)} where y = (words x)
data AllowedGates2qECC = AllowedGates2qECC{ allowedname2qECC::AllowedGateNames2q, allowedinvolvedQubitsECC::(QubitIndicesECC,QubitIndicesECC)} deriving (Eq)
instance Show AllowedGates2qECC where
        show x = show (allowedname2qECC x) ++ " on qubits " ++ show (allowedinvolvedQubitsECC x)
instance Read AllowedGates2qECC where
        read x = AllowedGate2qECC{ allowedname2qECC=read (head y), allowedinvolvedQubitsECC=read (last y)} where y = (words x)
default1qAllowedGateECC = AllowedGates1qECC{allowedname1q=PauliX,allowedinvolvedQubit=defaultECCIndex}
default2qAllowedGateECC = AllowedGates2qECC{allowedname2q=SqrtSwap,allowedinvolvedQubits=(defaultECCIndex,defaultECCIndex)}
type AllowedGatesECC = Either AllowedGates1qECC AllowedGates2qECC
instance Show AllowedGatesECC where
        show x
                | isLeft x = show (fromLeft default1qAllowedGateECC x)
                | otherwise = show (fromRight default2qAllowedGateECC x)
instance Read AllowedGatesECC where
        read x
                | y =="1q" = Left read x
                | y =="2q" = Right read x
                where y = head tail words x

data Gates1q = Gates1q{ name1q::GateNames1q, involvedQubit::QubitIndices}
instance Show Gates1q where
        show x = show (name1q x) ++ " on qubit " ++ show (involvedQubit x)
instance Read Gates1q where
        read x = Gate1q{ name1q=read (head y), involvedQubit=read (last y)} where y = (words x)
data Gates2q = Gates2q{ name2q::GateNames2q, involvedQubits2::(QubitIndices,QubitIndices)}
instance Show Gates2q where
        show x = show (name2q x) ++ " on qubits " ++ show (involvedQubits x)
instance Read Gates2q where
        read x = Gate2q{ name2q=read (head y), involvedQubits2=read (last y)} where y = (words x)
default1qGate = Gates1q{name1q=PauliX,involvedQubit=1}
default2qGate = Gates2q{name2q=SqrtSwap,involvedQubits2=(1,2)}
type Gates = Either Gates1q Gates2q
instance Show Gates where
        show x
                | isLeft x = show (fromLeft default1qGate x)
                | otherwise = show (fromRight default2qGate x)
instance Read Gates where
        read x
                | y =="1q" = Left read x
                | y =="2q" = Right read x
                where y = head tail words x

--A list of allowed gates
type ACircuitECC = [AllowedGatesECC]
type ACircuit = [AllowedGates]
type Circuit = [Gates]

--hardwareImplementation::Gates -> ACircuit
--hardwareImplementation (Left Gate1q{name1q=PauliX1,involvedQubit=x}) = [Left AllowedGate1q{name1q=APauliX1,involvedQubit=x}]
--hardwareImplementation (Left Gate1q{name1q=PauliY1,involvedQubit=x}) = [Left AllowedGate1q{name1q=APauliY1,involvedQubit=x}]
--hardwareImplementation (Left Gate1q{name1q=PauliZ1,involvedQubit=x}) = [Left AllowedGate1q{name1q=APauliZ1,involvedQubit=x}]
--hardwareImplementation (Right Gate2q{name2q=SqrtSwap2,involvedQubits=(x,y)}) = [Right AllowedGate2q{name1q=ASqrtSwap2,involvedQubits=(x,y)}]
--hardwareImplementation (Right Gate2q{name2q=CNOT2,involvedQubits=(x,y)}) = ... this one requires actual changes to do
--hardwareImplementation2::Circuit -> ACircuit
--hardwareImplementation2 [] = []
--hardwareImplementation2 x:xs = (hardwareImplementation x):(hardwareImplementation2 xs)

--applyErrorCorrection::[Gates] -> [ErrorCorrectionFlags] -> ACircuitECC

class GeneralGate g where
        name::g -> GateName
        arity::g -> Int
        involvedQubits::g->[Either QubitIndices QubitIndicesECC]
        --errorCorrectingCode::g -> ACircuitECC

instance GeneralGate AllowedGates where
        name x
                | isLeft x = toGateName1 $ toGateName1A $ allowedname1q $ fromLeft default1qAllowedGate x
                | otherwise = toGateName2 $ toGateName2A $ allowedname2q $ fromRight default2qAllowedGate x
        arity x
                | isLeft x = 1
                | otherwise = 2
		involvedQubits x
                | isLeft x = [Left (allowedinvolvedQubit $ fromLeft default1qAllowedGate x)]
                | otherwise = [Left fst z,Left snd z]
                where z = allowedinvolvedQubits $ fromRight default2qAllowedGate y
        --errorCorrectingCode x = use the ECCFlag one step at a time
instance GeneralGate AllowedGatesECC where
        name x
                | isLeft x = toGateName1 $ toGateName1A $ allowedname1qECC $ fromLeft default1qAllowedGateECC x
                | otherwise = toGateName2 $ toGateName2A $ allowedname2qECC $ fromRight default2qAllowedGateECC x
        arity x
                | isLeft x = 1
                | otherwise = 2
		involvedQubits x
                | isLeft x = [Right (allowedinvolvedQubitECC $ fromLeft default1qAllowedGateECC x)]
                | otherwise = [Right fst z,Right snd z]
                where z = allowedinvolvedQubitsECC $ fromRight default2qAllowedGateECC y
        --errorCorrectingCode x = [x]
instance GeneralGate Gates where
        name x
                | isLeft x = toGateName1 $ name1q $ fromLeft default1qAllowedGate x
                | otherwise = toGateName2 $ name2q $ fromRight default2qAllowedGate x
        arity x
                | isLeft x = 1
                | otherwise = 2
        involvedQubits x
                | isLeft x = [Left (involvedQubit $ fromLeft default1qGate x)]
                | otherwise = [Left fst z,Left snd z]
                where z = involvedQubits $ fromRight default2qGate y
        --errorCorrectingCode x = needs to both convert to allowed gates and implement the error Correcting Code

--errorCorrectingCode2::(GeneralGate g) => [g] -> ACircuitECC
--errorCorrectingCode2 [] = []
--errorCorrectingCode2 x:xs = (errorCorrectingCode x):(errorCorrectingCode2 xs)
				
--The most obvious way to ensure two gates commute is if they operate on different qubits
--In an overly cautious move say False if they might not commute, without examining content of gates
definitelyCommute::(GeneralGate g)=> g -> g -> Bool
definitelyCommute x y
    | inCommon==[] = True
	| otherwise = False
    where inCommon=(involvedQubits x) 'intersect' (involvedQubits y)

--type AllowedGateECCDAG = DirectedAcyclicGraph

--take a circuit and give the DAG for which gate must occur before the others
-- import from Data.Graph.DAG
--the helper function takes a single gate and the graph built so far and adds it to the top
-- an arrow from x->y in the graph indicates that x must be done before y. In particular if they commute no edge
--CompileStep1::ACircuitECC -> AllowedGateECCDAG
--CompileStep1
--CompileStep1Helper::AllowedGatesECC -> AllowedGateDAG -> AllowedGateDAG

--Step2 of compilation looks for certain subgraphs and replaces them with simpler versions
-- for example if there is a pair of pauliX gates that might be next to each other in a topological sort
-- then we should remove them both
-- elimination provides the substitution rule
-- eliminatePattern then uses that rule on the compilation DAG
--type EliminateablePattern = AllowedGateECCDAG
--elimination::EliminateablePattern -> AllowedGateECCDAG
--eliminatePattern::AllowedGateECCDAG -> EliminateablePattern -> AllowedGateECCDAG

--Take the DAG and return a possibly different circuit by doing a topological sort

--topSort:: Graph -> [Vertex] is implemented in Data.Graph import
--compileStep3::AllowedGateECCDAG -> ACircuitECC