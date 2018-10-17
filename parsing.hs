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
toInstr (stripPrefix "Q" -> Just restOfString) = (QuarterPhase1,readDigits restOfString)
toInstr (stripPrefix "QP" -> Just restOfString) = (QuarterPhase1,readDigits restOfString)
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