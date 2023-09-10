-- vim: foldmethod=marker

{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer
import Data.Void
import Data.Char (ord)
import Data.Text (pack, unpack, Text)
import qualified Data.Text as T (head, take, drop)
import qualified Data.Text.IO as TIO (readFile)
import Data.HashMap.Strict hiding (map, empty)
import Data.Bits
import qualified Data.ByteString as B (pack, writeFile)
import System.Environment

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
            ("iprint", 0x4),
            ("ftoi", 0x6), 
            ("itof", 0x7), 
            ("iadd", 0x8),
            ("isub", 0x9),
            ("imul", 0xa),
            ("pop", 0x10),
            ("wrpc", 0x21)
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
labelRef = SLabel <$> (some (choice [alphaNumChar, char '_', char '-', char '>', char '<']) >>= (return . pack))

literalString :: Parser [Constant]
literalString = (map (SInt . ord) . unpack) <$> (between (char '"') (char '"') $ takeWhile1P Nothing (/= '"'))

--- INSTRUCTIONS ---

branch :: Parser Instruction
branch = choice $ 
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
db = Db <$> (string' ".db" *> space1 *> ((lexw int <|> lexw labelRef) `sepBy` (lexw $ char ',')))

instruction :: Parser Instruction
instruction = Instr <$> (choice $ (\x->(string' . fst $ x) >>= \_->return $ snd x) <$> arglessInstrs)

labelDef :: Parser Instruction
labelDef = LabelDef <$> (some alphaNumChar <* char ':' >>= (return . pack))

stringDef :: Parser Instruction
stringDef = StringDef <$> literalString

--- MAIN PARSER ---

statement :: Parser [Instruction]
statement = choice [try labelDef, db, stringDef, branch, load, store, push, instruction] `sepBy1` sc'
--- }}}

--- BINARY GENERATION ---

sizeOfInstr :: Instruction -> Int
sizeOfInstr r = case r of
                  Push (SInt num) -> if num > 0xffff then 5 else 3
                  Push (SLabel text) -> 3
                  Instr _  -> 1
                  Branch _ _ -> 3
                  Load _ -> 2
                  Store _ -> 2
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
dbToBytes l (SLabel x:xs) = if T.take 2 x == ">>" then
                              ((.&. 0xff) $ unpackLabel l $ T.drop 2 x):dbToBytes l xs
                          else if T.take 2 x == "<<" then
                                (shiftR (unpackLabel l $ T.drop 2 x) 24 .&. 0xff):dbToBytes l xs
                               else if T.head x == '>' then
                                 (shiftR (unpackLabel l $ T.drop 2 x) 8 .&. 0xff):dbToBytes l xs
                                    else if T.head x == '<' then
                                        (shiftR (unpackLabel l $ T.drop 2 x) 16 .&. 0xff):dbToBytes l xs
                                            else error $ "label "++ unpack x ++ " is not 1 byte!"
dbToBytes _ [] = []

intToBytes :: Int -> [Int]
intToBytes k = case (k < 0, abs k < 0xffff) of
                 (True, True) -> let n = 0x10000 + k in [n .&. 0xff, (shiftR n 8) .&. 0xff]
                 (True, False) -> let n = 0x100000000 + k in [n .&. 0xff, (shiftR n 8) .&. 0xff, (shiftR n 16) .&. 0xff, (shiftR n 24) .&. 0xff]
                 (False, True) -> [k .&. 0xff, (shiftR k 8) .&. 0xff]
                 (False, False) -> [k .&. 0xff, (shiftR k 8) .&. 0xff, (shiftR k 16) .&. 0xff, (shiftR k 24) .&. 0xff]

genBinary' :: Int -> [Instruction] -> HashMap Text Int -> [Int]
genBinary' pos (inst:instrs) labels = bytes ++ rest
    where
        rest = genBinary' (pos + sizeOfInstr inst) instrs labels
        bytes = case inst of
            Push (SInt num) -> 0x3 : intToBytes num
                                --num .&. 0xff, (shiftR num 8) .&. 0xff] ++ 
                                --(if num > 0xffff then [(shiftR num 16) .&. 0xff, (shiftR num 24) .&. 0xff] else [])
            Push (SLabel text) -> let labelValue = unpackLabel labels text 
                                   in 0x3 : (take 2 $ intToBytes labelValue)-- labelValue .&. 0xff, (shiftR labelValue 8) .&. 0xff]
            Instr num -> [num]
            Branch num (SLabel text) -> let labelValue = unpackLabel labels text - pos - 3
                                         in num : (take 2 $ intToBytes labelValue)-- [num, labelValue .&. 0xff, (shiftR labelValue 8) .&. 0xff]
            Load (SInt x) -> [0x1f, x .&. 0xff]
            Store (SInt x) -> [0x1e, x .&. 0xff]
            Db x -> dbToBytes labels x
            StringDef x -> dbToBytes labels x
            _ -> undefined
genBinary' _ [] _ = []

genBinary :: [Instruction] -> [Int]
genBinary = uncurry (genBinary' 0) . resolveLabels 0

--- MAINLOOP ---

main :: IO ()
main = do
    a <- getArgs
    let k = length a
    if k == 1 || k == 2 then do
        let outFile = if k == 2 then a !! 1 else (head a) ++ ".out"
        prg <- TIO.readFile $ head a
        either (putStr . errorBundlePretty) (B.writeFile outFile . B.pack . map fromIntegral . genBinary) $ parse (sc *> statement <* eof) (head a) prg
    else
        error "wrong number of args!"