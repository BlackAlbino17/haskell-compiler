module Parser where 
import Container 


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
          | LEQ Aexp Aexp
          | Aexp
          deriving (Eq, Show)


compA :: Aexp -> Code
compA (Num n)    = [Push n]
compA (Var x)    = [Fetch x]  -- Handle variable case
compA (ADD e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (SUB e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (MULT e1 e2) = compA e1 ++ compA e2 ++ [Mult]


compB :: Bexp -> Code
compB TRUE      = [Tru]
compB FALSE     = [Fals]
compB (NOT b)   = compB b ++ [Neg]
compB (AND b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (LEQ e1 e2) = compA e1 ++ compA e2 ++ [Le]

data Stm = Assign String Aexp
         | Seq Stm Stm
         | IfThenElse Bexp Stm Stm
         | While Bexp Stm
         deriving (Eq, Show)


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




testCompA :: Aexp
testCompA = (SUB (MULT (ADD (Num 1) (Num 2)) (Num 3)) (Num 4))  

exampleBexp :: Bexp
exampleBexp = AND (LEQ (Num (-5)) (Num 10)) (NOT TRUE)

exampleProgram :: [Stm]
exampleProgram = [Assign "x" (Num 5), Assign "y" (ADD (Var "x") (Num 3))]