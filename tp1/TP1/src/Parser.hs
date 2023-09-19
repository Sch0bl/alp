module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "repeat", "skip", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expressiones enteras
-----------------------------------

seqExp :: Parser (Exp Int -> Exp Int -> Exp Int)
seqExp = do reservedOp lis ","
            return ESeq

assgExp :: Parser (Exp Int)
assgExp = do
  var <- identifier lis  
  reservedOp lis "="
  e <- intexp2
  return (EAssgn var e)

plusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
plusExp = do reservedOp lis "+"
             return Plus

minusExp :: Parser (Exp Int -> Exp Int -> Exp Int)
minusExp = do reservedOp lis "-"
              return Minus

divExp :: Parser (Exp Int -> Exp Int -> Exp Int)
divExp = do reservedOp lis "/"
            return Div

timesExp :: Parser (Exp Int -> Exp Int -> Exp Int)
timesExp = do reservedOp lis "*"
              return Times

umExp :: Parser (Exp Int)
umExp = do reservedOp lis "-"
           e <- intexp5
           return (UMinus e)

consExp :: Parser (Exp Int)
consExp = do nat <- natural lis
             return (Const (fromIntegral nat))

varExp :: Parser (Exp Int)
varExp = do var <- identifier lis
            return (Var var)

intexp0 :: Parser (Exp Int)
intexp0 = chainl1 intexp1 seqExp

intexp1 :: Parser (Exp Int)
intexp1 = (try assgExp) <|> intexp2


intexp2 :: Parser (Exp Int)
intexp2 = chainl1 intexp3 (minusExp <|> plusExp)
          -- <|> 
          -- intexp3

intexp3 :: Parser (Exp Int)
intexp3 = chainl1 intexp4 (timesExp <|> divExp)
          -- <|> 
          -- intexp4

intexp4 :: Parser (Exp Int)
intexp4 = umExp 
          <|>
          intexp5

intexp5 :: Parser (Exp Int)
intexp5 = parens lis intexp0
          <|>
          varExp
          <|> 
          consExp
          
intexp :: Parser (Exp Int)
intexp = intexp0

-----------------------------------
--- Parser de expressiones booleanas
-----------------------------------
orBool :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orBool = do reservedOp lis "||"
            return Or

andBool :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
andBool = do reservedOp lis "&&"
             return And

notBool :: Parser (Exp Bool)
notBool = do reservedOp lis "!"
             be <- boolexp3
             return (Not be)

eqBool :: Parser (Exp Int -> Exp Bool)
eqBool = do reservedOp lis "=="
            ie <- intexp
            return (\x ->(Eq x ie))

neqBool :: Parser (Exp Int -> Exp Bool)
neqBool = do reservedOp lis "/="
             ie <- intexp
             return (\x -> (NEq x ie))

ltBool :: Parser (Exp Int -> Exp Bool)
ltBool = do reservedOp lis "<"
            ie <- intexp
            return (\x -> (Lt x ie))

gtBool :: Parser (Exp Int -> Exp Bool)
gtBool = do reservedOp lis ">"
            ie <- intexp
            return (\x -> (Gt x ie))

trueBool :: Parser (Exp Bool)
trueBool = do reserved lis "true"
              return BTrue

falseBool :: Parser (Exp Bool)
falseBool = do  reserved lis "false"
                return BFalse

boolexp0 :: Parser (Exp Bool)
boolexp0 = chainl1 boolexp1 orBool
           -- <|>
           -- boolexp1

boolexp1 :: Parser (Exp Bool)
boolexp1 = chainl1 boolexp2 andBool
           -- <|>
           -- boolexp2

boolexp2 :: Parser (Exp Bool)
boolexp2 = try notBool
           <|> 
           boolexp3

parase :: Parser (Exp Bool)
parase = do ie1 <- intexp
            f <- gtBool <|> ltBool <|> eqBool <|> neqBool 
            return $ f ie1

boolexp3 :: Parser (Exp Bool)
boolexp3 = try parase
           <|>
           parens lis boolexp0
           <|>
           trueBool
           <|>
           falseBool

boolexp :: Parser (Exp Bool)
boolexp = boolexp0

-----------------------------------
--- Parser de comandos
-----------------------------------
seqComm :: Parser (Comm -> Comm -> Comm)
seqComm = do reservedOp lis ";"
             return Seq

letComm :: Parser Comm
letComm = do var <- identifier lis 
             reservedOp lis "="
             ie <- intexp
             return (Let var ie)

skipComm :: Parser Comm
skipComm = do reserved lis "skip"
              return Skip

iteComm :: Parser Comm
iteComm = do reserved lis "if"
             be <- boolexp
             cm <- braces lis comm
             (do reserved lis "else"
                 cme <- comm
                 return (IfThenElse be cm cme)
                 <|>
                 return (IfThenElse be cm Skip))
         
ruComm :: Parser Comm
ruComm = do reserved lis "repeat" 
            cm <- braces lis comm
            reserved lis "until"
            be <- boolexp
            return (Repeat cm be)

comm :: Parser Comm
comm = comm0

comm0 :: Parser Comm
comm0 = chainl1 comm1 seqComm
        <|>
        comm1

comm1 :: Parser Comm
comm1 = skipComm 
        <|>
        letComm 
        <|> 
        iteComm
        <|> 
        ruComm

------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
