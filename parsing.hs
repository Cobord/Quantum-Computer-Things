{-# LANGUAGE ViewPatterns #-}
import Data.List
import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell = 
    do char '"'
       content <- many quotedChar
       char '"' <?> "quote at end of cell"
       return content

quotedChar =
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r

data InstructionSet = PauliX1 | PauliY1 | PauliZ1 | Hadamard1 | QuarterPhase1 | SqrtSwap2 | CNOT2 | Swap2 | Tof3 | NA deriving (Eq,Enum,Read,Show)

readDigits :: String -> [Int]
readDigits xs = map (\x -> read [x] :: Int) xs

toInstr :: String -> (InstructionSet,[Int])
toInstr (stripPrefix "X" -> Just restOfString) = (PauliX1,readDigits restOfString)
toInstr (stripPrefix "Y" -> Just restOfString) = (PauliY1,readDigits restOfString)
toInstr (stripPrefix "Z" -> Just restOfString) = (PauliZ1,readDigits restOfString)
toInstr (stripPrefix "H" -> Just restOfString) = (Hadamard1,readDigits restOfString)
toInstr (stripPrefix "QP" -> Just restOfString) = (QuarterPhase1,readDigits restOfString)
toInstr (stripPrefix "Q" -> Just restOfString) = (QuarterPhase1,readDigits restOfString)
toInstr (stripPrefix "SQRTSWAP" -> Just restOfString) = (SqrtSwap2,readDigits restOfString)
toInstr (stripPrefix "CNOT" -> Just restOfString) = (CNOT2,readDigits restOfString)
toInstr (stripPrefix "SWAP" -> Just restOfString) = (Swap2,readDigits restOfString)
toInstr (stripPrefix "TOF" -> Just restOfString) = (Tof3,readDigits restOfString)
toInstr _ = (NA,[])

readProgramHelper :: Either ParseError [[String]] -> Either ParseError [[(InstructionSet,[Int])]]
readProgramHelper (Left x) = (Left x)
readProgramHelper (Right y) = Right (fmap (fmap toInstr) y)

readProgram :: String -> Either ParseError [[(InstructionSet,[Int])]]
readProgram input = readProgramHelper $ parseCSV input

--Example
--readProgram "X1,Y2,H3,SWAP12\n"
-- will return Right [[(PauliX1,[1]),(PauliY1,[2]),(Hadamard1,[3]),(Swap2,[1,2])]]
-- there is a single line of the program so of form [[]] instead of [[],[]] which it would if there were multiple lines with multiple \n

--TODO apply IO in order to get the string from a file instead of an argument to the function readProgram
--TODO this program assumes there are fewer than 10 logical qubits so that 10 is interpreted as qubits 1 and 0 instead of qubit 10
--     fixing this will entail formatting the input differently. Maybe use something like H3,SWAP10;2 so swaps 10 and 2.
--     comma is already reserved to separate differnt gates, need another separator for qubit indices