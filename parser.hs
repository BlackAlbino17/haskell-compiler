module Parser where 
import Container 


-- Part 2
-- TODO: Define the types Aexp, Bexp, Stm and Program

data Aexp = ADD Aexp Aexp
          | SUB Aexp Aexp
          | MULT Aexp Aexp
          | Num Integer
          deriving (Eq, Show)

data Bexp = TRUE
          | FALSE
          | NOT Bexp
          | AND Bexp Bexp
          | LEQ Aexp Aexp
          | Aexp
          deriving (Eq, Show)

compA :: Aexp -> Code
compA (Parser.Num n)    = [Push n]
compA (ADD e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (SUB e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (MULT e1 e2) = compA e1 ++ compA e2 ++ [Mult]

compB :: Bexp -> Code
compB TRUE      = [Tru]
compB FALSE     = [Fals]
compB (NOT b)   = compB b ++ [Neg]
compB (AND b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LEQ e1 e2) = compA e1 ++ compA e2 ++ [Le]


-- compile :: Program -> Code
compile = undefined -- TODO


-- parse :: String -> Program
--parse = undefined -- TODO

test :: Aexp
test = (MULT (ADD (Num 1) (Num 2)) (Num 3))    
test1 = (SUB (MULT (ADD (Num 1) (Num 2)) (Num 3)) (Num 4))

exampleBexp :: Bexp
exampleBexp = AND (LEQ (Num 5) (Num 10)) (NOT TRUE)