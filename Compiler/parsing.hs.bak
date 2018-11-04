{-# LANGUAGE ViewPatterns #-}
import Data.List
import Text.ParserCombinators.Parsec
import System.IO  
import Control.Monad
import Data.Typeable
import qualified Data.List.Split as Splitter

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

data InstructionSet = PauliX1 | PauliY1 | PauliZ1 | Hadamard1 | QuarterPhase1 | SqrtSwap2 | CNOT2 | Swap2 | Tof3 | NA deriving (Eq,Enum,Read,Show)

readDigits :: String -> [Int]
readDigits xs = map (\x -> read [x] :: Int) xs

readDigits2 :: String -> [Int]
readDigits2 xs = map read (Splitter.splitOn ";" xs)

toInstr :: String -> (InstructionSet,[Int])
toInstr (stripPrefix "X" -> Just restOfString) = (PauliX1,readDigits2 restOfString)
toInstr (stripPrefix "Y" -> Just restOfString) = (PauliY1,readDigits2 restOfString)
toInstr (stripPrefix "Z" -> Just restOfString) = (PauliZ1,readDigits2 restOfString)
toInstr (stripPrefix "H" -> Just restOfString) = (Hadamard1,readDigits2 restOfString)
toInstr (stripPrefix "QP" -> Just restOfString) = (QuarterPhase1,readDigits2 restOfString)
toInstr (stripPrefix "Q" -> Just restOfString) = (QuarterPhase1,readDigits2 restOfString)
toInstr (stripPrefix "SQRTSWAP" -> Just restOfString) = (SqrtSwap2,readDigits2 restOfString)
toInstr (stripPrefix "CNOT" -> Just restOfString) = (CNOT2,readDigits2 restOfString)
toInstr (stripPrefix "SWAP" -> Just restOfString) = (Swap2,readDigits2 restOfString)
toInstr (stripPrefix "TOF" -> Just restOfString) = (Tof3,readDigits2 restOfString)
toInstr _ = (NA,[])

readProgramHelper :: Either ParseError [[String]] -> Either ParseError [[(InstructionSet,[Int])]]
readProgramHelper (Left x) = (Left x)
readProgramHelper (Right y) = Right (fmap (fmap toInstr) y)

readProgram :: String -> Either ParseError [[(InstructionSet,[Int])]]
readProgram input = readProgramHelper $ parseCSV input

writeProgramHelper2 :: [Int] -> String
writeProgramHelper2 [] = ""
writeProgramHelper2 (x:[]) = show x
writeProgramHelper2 (x:xs) = show x ++ ";" ++ (writeProgramHelper2 xs)

writeProgramHelper :: (InstructionSet,[Int]) -> String
writeProgramHelper (PauliX1,onDigits) = "X"++(writeProgramHelper2 onDigits)
writeProgramHelper (PauliY1,onDigits) = "Y"++(writeProgramHelper2 onDigits)
writeProgramHelper (PauliZ1,onDigits) = "Z"++(writeProgramHelper2 onDigits)
writeProgramHelper (Hadamard1,onDigits) = "H"++(writeProgramHelper2 onDigits)
writeProgramHelper (QuarterPhase1,onDigits) = "QP"++(writeProgramHelper2 onDigits)
writeProgramHelper (SqrtSwap2,onDigits) = "SQRTSWAP"++(writeProgramHelper2 onDigits)
writeProgramHelper (CNOT2,onDigits) = "CNOT"++(writeProgramHelper2 onDigits)
writeProgramHelper (Swap2,onDigits) = "SWAP"++(writeProgramHelper2 onDigits)
writeProgramHelper (Tof3,onDigits) = "TOF"++(writeProgramHelper2 onDigits)

writeProgram :: [(InstructionSet,[Int])] -> String
writeProgram [] = "\n"
writeProgram (x:[]) = (writeProgramHelper x) ++ "\n"
writeProgram (x:xs) = (writeProgramHelper x) ++ "," ++ (writeProgram xs)

writeManyPrograms :: Either ParseError [[(InstructionSet,[Int])]] -> String
writeManyPrograms (Left x) = show x
writeManyPrograms (Right x) = foldl (++) "" [writeProgram y | y <- x]

--Example
--readProgram "Z3,H1,Y2,H4,TOF1;2;4,CNOT1;52\n"
-- will return Right [[(PauliZ1,[3]),(Hadamard1,[1]),(PauliY1,[2]),(Hadamard1,[4]),(Tof3,[1,2,4]),(CNOT2,[1,52])]]
-- there is a single line of the program so of form [[]] instead of [[],[]] which it would if there were multiple lines with multiple \n

main = do  
        contents <- readFile "test.txt"
        let parsedPrograms = map readProgram . map (\x -> x ++ "\n") . words $ contents
        let shownParsed = [writeManyPrograms x | x <- parsedPrograms]
        let shownParsed2 = [(show x) ++ "\n" | x <- parsedPrograms]
        writeFile "parsed_test.txt" (foldl (++) "" shownParsed2)
        writeFile "refactored_test.txt" (foldl (++) "" shownParsed)

-- The use case for this will be the naive program will be written in test.txt, it will get modified by something like compilerCurrent.hs
-- and then written into refactored_test.txt
-- the parsed_test.txt shows that it correctly read the data so that it could go into (GateData None) from compilerCurrent.hs