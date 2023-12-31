module Parser where

import Container
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser, OperatorTable)
import Text.Parsec.Prim (ParsecT)
import Control.Monad.Identity (Identity)


-- Part 2
-- TODO: Define the types Aexp, Bexp, Stm and Program

type Program = [Stm]

data Aexp = ADD Aexp Aexp
          | SUB Aexp Aexp
          | MULT Aexp Aexp
          | Num Integer
          | Var String
          deriving (Eq, Show)

data Bexp = TRUE
          | FALSE
          | NOT Bexp
          | AND Bexp Bexp
       --   | LEQ Aexp Aexp  
          deriving (Eq, Show)

data Stm = Assign String Aexp
         | Seq Stm Stm
         | IfThenElse Bexp Stm Stm
         | While Bexp Stm
         deriving (Eq, Show)


compA :: Aexp -> Code
compA (Num n)    = [Push n]
compA (Var x)    = [Fetch x]  
compA (ADD e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (SUB e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (MULT e1 e2) = compA e1 ++ compA e2 ++ [Mult]


compB :: Bexp -> Code
compB TRUE      = [Tru]
compB FALSE     = [Fals]
compB (NOT b)   = compB b ++ [Neg]
compB (AND b1 b2) = compB b1 ++ compB b2 ++ [And]
--compB (LEQ e1 e2) = compA e1 ++ compA e2 ++ [Le]



compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq stmt1 stmt2 -> compile [stmt1] ++ compile [stmt2] ++ compile rest
  IfThenElse cond thenStmt elseStmt ->
    compB cond ++ [Branch (compile [thenStmt]) (compile [elseStmt])] ++ compile rest
  While cond body ->
    compile [body, While cond body] ++ [Branch (compile [body, While cond body]) [Noop]] ++ compile rest



-- parse :: String -> Program
--parse = undefined -- TODO
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

operator :: Parser String
operator = Token.operator lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

lexeme :: Parser a -> Parser a
lexeme p = p <* Token.whiteSpace lexer



aexpParser :: Parser Aexp
aexpParser = buildExpressionParser aexpOperators aexpTerm
bexpParser :: Parser Bexp
bexpParser = buildExpressionParser bexpOperators bexpTerm




aexpOperators :: OperatorTable String () Identity Aexp
aexpOperators =
  [ [Infix (reserved "*" >> return MULT) AssocLeft],
    [Infix (reserved "+" >> return ADD) AssocLeft,
     Infix (reserved "-" >> return SUB) AssocLeft]
  ]

bexpOperators :: OperatorTable String () Identity Bexp
bexpOperators =
  [ [Prefix (reserved "not" >> return NOT)],
    [Infix (reserved "and" >> return AND) AssocLeft]
 --   [Infix (reserved "<=" >> return LEQ) AssocLeft]
  ]





aexpTerm :: Parser Aexp
aexpTerm = parens aexpParser <|> Var <$> identifier <|> Num <$> integer

bexpTerm :: Parser Bexp
bexpTerm = parens bexpParser <|> (reserved "True" >> return TRUE) <|> (reserved "False" >> return FALSE)



stmParser :: Parser Stm
stmParser = assignParser <|> seqParser <|> ifThenElseParser <|> whileParser

assignParser :: Parser Stm
assignParser = do
  var <- identifier
  reserved ":="
  expr <- aexpParser
  optional (reserved ";") 
  return $ Assign var expr

seqParser :: Parser Stm
seqParser = do
  stmts <- stmParser `sepEndBy1` reserved ";"
  optional spaces
  return $ foldr1 Seq stmts

ifThenElseParser :: Parser Stm
ifThenElseParser = do
  reserved "if"
  cond <- bexpParser
  reserved "then"
  thenBranch <- stmParser
  reserved "else"
  elseBranch <- stmParser
  optional (reserved ";") 
  return $ IfThenElse cond thenBranch elseBranch

whileParser :: Parser Stm
whileParser = do
  reserved "while"
  cond <- bexpParser
  body <- stmParser
  optional (reserved ";") 
  return $ While cond body




parse :: String -> Program
parse input = case Text.Parsec.parse (semiSep stmParser) "" input of
  Left err -> error (show err)
  Right program -> program


programParser :: Parser Program
programParser = semiSep stmParser












