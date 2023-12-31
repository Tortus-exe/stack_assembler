-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Debug
import Data.Void
import Data.Char (ord)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T (head, take, drop)
import qualified Data.Text.IO as TIO (readFile)
import Data.HashMap.Strict hiding (map, empty)
import Data.Bits
import qualified Data.ByteString as B (pack, writeFile)
import System.Environment
import System.IO.Unsafe

type Parser = Parsec Void Text

data Constant = SInt Int
              | SFloat Float
              | SLabel Text
              deriving (Show, Eq)

data Instruction = Push Constant
                 | Instr Int
                 | Branch Int Constant
                 | Load Constant
                 | Store Constant
                 | LabelDef Text
                 | Db [Constant]
                 | StringDef [Constant]
                 deriving (Show, Eq)

--- UTIL --- {{{

sc' :: Parser ()
sc' = skipSome $ choice [hidden space1, hidden $ skipLineComment ";"]

sc :: Parser ()
sc = space space1 (skipLineComment ";") empty

lexw :: Parser a -> Parser a
lexw = lexeme sc

arglessInstrs :: [(Text, Int)]
arglessInstrs = [
    ("nop", 0x00), 
    ("vtaskdelay", 0x01), 
    ("iprint", 0x04), 
    ("fprint", 0x05), 
    ("ftoi", 0x06), 
    ("itof", 0x07), 
    ("iadd", 0x08), 
    ("isub", 0x09), 
    ("imul", 0x0a), 
    ("idiv", 0x0b), 
    ("fadd", 0x0c), 
    ("fsub", 0x0d), 
    ("fmul", 0x0e), 
    ("fdiv", 0x0f), 
    ("pop", 0x10), 
    ("swap", 0x11), 
    ("dup", 0x19), 
    ("and", 0x1a), 
    ("or", 0x1b), 
    ("xor", 0x1c), 
    ("not", 0x1d), 
    ("phpc", 0x20), 
    ("wrpc", 0x21), 
    ("phsp", 0x22), 
    ("wrsp", 0x23), 
    ("jsrs", 0x25), 
    ("pusha", 0x27), 
    ("pushaw", 0x26), 
    ("pushab", 0x28), 
    ("sprint", 0x29), 
    ("lsl", 0x2a), 
    ("lsr", 0x2b), 
    ("mod", 0x2c),
    ("negf", 0x2f),
    ("neg", 0x2e),
    ("malloca", 0x38),
    ("freea", 0x39),
    ("indexa", 0x3b),
    ("indexab", 0x3a),
    ("indexaw", 0x3c),
    ("writea", 0x3d),
    ("halt", 0xff)
                ]

branches :: [(Text, Int)]
branches = [("beq", 0x12),
            ("bgt", 0x13),
            ("blt", 0x14),
            ("bge", 0x15),
            ("ble", 0x16),
            ("bne", 0x17),
            ("jmp", 0x18),
            ("jsr", 0x24)]

--- NUMBERS ---

int :: Parser Constant
int = SInt <$> 
    (((string "0x" <|> string "0X") *> hexadecimal) <|> 
        decimal
    )

labelRef :: Parser Constant
labelRef = SLabel <$> (some (choice [alphaNumChar, char '_', char '-', char '>', char '<']) <* lookAhead sc >>= (return . pack))

literalString :: Parser [Constant]
literalString = (map (SInt . ord) . unpack) <$> (between (char '"') (char '"') $ takeWhile1P Nothing (/= '"'))

sepByLazy :: Parser a -> Parser sep -> Parser [a]
sepByLazy p sep = do
  r <- optional p
  case r of
    Nothing -> return []
    Just  x -> (x:) <$> many (try $ sep >> p)

--- INSTRUCTIONS ---

branch :: Parser Instruction
branch = label "branch instruction" $ choice $ 
    (\x->do 
        _ <- (string' . fst $ x) <* space1
        Branch (snd x) <$> labelRef
    ) <$> branches

push :: Parser Instruction
push = Push <$> do
    _ <- string' "push" <* space1
    int <|> labelRef

load :: Parser Instruction
load = Load <$> do
    _ <- string' "load" <* space1
    int

store :: Parser Instruction
store = Store <$> do
    _ <- string' "store" <* space1
    int

db :: Parser Instruction
db = Db <$> (string' ".db" *> space1 *> ((int <|> labelRef) `sepBy` ((char ','))))

instruction :: Parser Instruction
instruction = Instr <$> (choice $ (\x->(string' . fst $ x) >>= \_->return $ snd x) <$> arglessInstrs) <?> "instruction"

labelDef :: Parser Instruction
labelDef = LabelDef <$> ((some (choice [alphaNumChar, char '_', char '-', char '>', char '<']) <* char ':' >>= (return . pack)) <?> "label")

stringDef :: Parser Instruction
stringDef = StringDef <$> literalString

--- MAIN PARSER ---

statement :: Parser [Instruction]
statement = choice [
            try labelDef, try db, try stringDef, try branch, try load, try store, try push, instruction
                   ] `sepEndBy1` sc'
--- }}}
--- BINARY GENERATION --- {{{

sizeOfInstr :: Instruction -> Int
sizeOfInstr r = case r of
                  Push (SInt num) | num > 0xffff -> 5
                                  | num > 0xff -> 3
                                  | num > 0x7 -> 2
                                  | otherwise -> 1
                  Push (SLabel text) -> 3
                  Instr _  -> 1
                  Branch _ _ -> 3
                  Load (SInt x) -> if x > 4 then 2 else 1
                  Store (SInt x) -> if x > 4 then 2 else 1
                  Db x -> length x
                  StringDef x -> length x
                  _ -> undefined

resolveLabels :: Int -> [Instruction] -> ([Instruction], HashMap Text Int)
resolveLabels pos (LabelDef name:instrs) = let (withoutLabels, defdLabels) = resolveLabels pos instrs
                                        in (withoutLabels, insert name pos defdLabels)
resolveLabels pos (instr:instrs) = let (withoutLabels, defdLabels) = resolveLabels (pos + (sizeOfInstr instr)) instrs
                                    in (instr:withoutLabels, defdLabels)
resolveLabels pos [] = ([], fromList [])

unpackLabel :: HashMap Text Int -> Text -> Int
unpackLabel labels text = case labels !? text of
                            Nothing -> error $ "illegal label: " ++ unpack text
                            Just x -> x

dbToBytes :: HashMap Text Int -> [Constant] -> [Int]
dbToBytes l (SInt x:xs) = (x .&. 0xff):dbToBytes l xs
dbToBytes l (SLabel x:xs) | T.take 2 x == ">>" = ((.&. 0xff) $ unpackLabel l $ T.drop 2 x):dbToBytes l xs
                          | T.take 2 x == "<<" = (shiftR (unpackLabel l $ T.drop 2 x) 24 .&. 0xff):dbToBytes l xs
                          | T.head x == '>' = (shiftR (unpackLabel l $ T.drop 1 x) 8 .&. 0xff):dbToBytes l xs
                          | T.head x == '<' = (shiftR (unpackLabel l $ T.drop 1 x) 16 .&. 0xff):dbToBytes l xs
                          | otherwise = error $ "label "++ unpack x ++ " is not 1 byte!"
dbToBytes _ [] = []

intToBytes :: Int -> [Int]
intToBytes k = case (k < 0, abs k < 0xffff, abs k < 0xff) of
                 (True, True, False) -> let n = 0x10000 + k in [n .&. 0xff, (shiftR n 8) .&. 0xff]
                 (True, False, False) -> let n = 0x100000000 + k in [n .&. 0xff, (shiftR n 8) .&. 0xff, (shiftR n 16) .&. 0xff, (shiftR n 24) .&. 0xff]
                 (False, True, False) -> [k .&. 0xff, (shiftR k 8) .&. 0xff]
                 (False, False, False) -> [k .&. 0xff, (shiftR k 8) .&. 0xff, (shiftR k 16) .&. 0xff, (shiftR k 24) .&. 0xff]
                 (True, True, True) -> let n = 0x100 + k in [n .&. 0xff]
                 (False, True, True) -> [k .&. 0xff]
                 _ -> error "somehow a number is greater than 0xffff, yet less than 0xff"

to16Bit :: [Int] -> [Int]
to16Bit k = if length k >= 2 then k else
                if head k >= 0x80 then
                    k ++ [0xff]
                else
                    k ++ [0x00]

genBinary' :: Int -> [Instruction] -> HashMap Text Int -> [Int]
genBinary' pos (inst:instrs) labels = bytes ++ rest
    where
        rest = genBinary' (pos + sizeOfInstr inst) instrs labels
        bytes = case inst of
            Push (SInt num) -> if num >= 0 && num <= 7 then [0x30+num]
                               else
                                   let k = intToBytes num
                                   in case length k of
                                        4 -> 2 : k
                                        2 -> 3 : k
                                        1 -> 0x2d : k
                                --num .&. 0xff, (shiftR num 8) .&. 0xff] ++ 
                                --(if num > 0xffff then [(shiftR num 16) .&. 0xff, (shiftR num 24) .&. 0xff] else [])
            Push (SLabel text) -> let labelValue = unpackLabel labels text 
                                   in 0x3 : (take 2 $ to16Bit $ intToBytes labelValue)-- labelValue .&. 0xff, (shiftR labelValue 8) .&. 0xff]
            Instr num -> [num]
            Branch num (SLabel text) -> let labelValue = unpackLabel labels text - pos - 3
                                         in num : (take 2 $ to16Bit $ intToBytes labelValue)-- [num, labelValue .&. 0xff, (shiftR labelValue 8) .&. 0xff]
            Load (SInt x) -> if x <= 4 && x >= 0 then [0x43+x] else [0x1f, x .&. 0xff]
            Store (SInt x) -> if x <= 4 && x >= 0 then [0x3e+x] else [0x1e, x .&. 0xff]
            Db x -> dbToBytes labels x
            StringDef x -> dbToBytes labels x
            _ -> undefined
genBinary' _ [] _ = []

genBinary :: [Instruction] -> [Int]
genBinary = uncurry (genBinary' 0) . resolveLabels 0
---}}}
--- MAINLOOP ---

main :: IO ()
main = do
    a <- getArgs
    let k = length a
    let outFile = case k of
                    1 -> (head a) ++ ".out"
                    2 -> a !! 1
                    _ -> error "wrong number of args!"
        -- let outFile = if k == 2 then a !! 1 else (head a) ++ ".out"
    prg <- TIO.readFile $ head a
    either (putStr . errorBundlePretty) 
        -- (print) $ 
        (B.writeFile outFile . B.pack . map fromIntegral . genBinary) $ 
            parse (sc *> statement <* eof) (head a) prg