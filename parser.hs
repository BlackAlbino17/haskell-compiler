module Parser where 
import Container 
import Data.Char (isSpace, isDigit)
import Data.List (elemIndex)
import Text.Read (readMaybe)




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
          | Eq Aexp Aexp
          | EqBool Bexp Bexp
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
         | ParserWhile Bexp [Stm]
         | ParserCond Bexp [Stm] [Stm]
         deriving (Eq, Show)


compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq stmt1 stmt2 -> compile [stmt1] ++ compile [stmt2] ++ compile rest
  IfThenElse evenBCond thenStmt elseStmt ->
    compB evenBCond ++ [Branch (compile [thenStmt]) (compile [elseStmt])] ++ compile rest
  While evenBCond body ->
    compile [body, While evenBCond body] ++ [Branch (compile [body, While evenBCond body]) [Noop]] ++ compile rest



-- parse :: String -> Program
--parse = undefined -- TODO

---------------------------------------------  LEXER e Get Aux ------------------------------------------------------------

getValue :: Num a => Maybe a -> a
getValue (Just a) = a+1
getValue Nothing = error "Throw error getValue(Value not found)"

lexer :: String -> [String]
lexer = reverse . go []
  where
    go :: [String] -> String -> [String]
    go acc [] = acc
    go acc (c:cs)
      | isSpace c = go acc cs
      | isDigit c = let (token, rest) = span isDigit (c:cs)
                    in go (token : acc) rest
      | c `elem` "();+" = go ([c] : acc) cs
      | c == '*'   = go ("*" : acc) cs
      | otherwise = let (token, rest) = span (\x -> not (isSpace x || isDigit x || x `elem` "();+*")) (c:cs)
                    in go (token : acc) rest



parse :: String -> Program
parse str = parsing(lexer str) []

parsing :: [String] -> [Stm] -> [Stm]
parsing [] stm = stm -- default



--parsing dos assigns
parsing (a:":=":rest) stm = let x = (getValue (elemIndex ";" (a:":=":rest)))
                              in case parseSumOrProdOrIntOrPar (drop 2 (take (x-1) (a:":=":rest))) of
                                Just (expr,[]) -> parsing (drop x (a:":=":rest)) (stm++[(Assign a (expr))])
                                Nothing -> error "Parse Error"
                                _ -> error "Parse Error"

-- parsing ponto virgula
parsing(";":rest) statement = parsing rest statement

parsing ("(":rest) statement =
  let closeParenIndex = getValue (elemIndex ")" ("(":rest))
      arrayAfterCloseParen = drop closeParenIndex ("(":rest)
      arrayInsideParens = drop 1 (take (closeParenIndex - 1) ("(":rest))
  in parsing arrayAfterCloseParen (statement ++ parsing arrayInsideParens [])

--parsing loops
parsing("while":rest) statement = 
  let ele = getValue(elemIndex "do" ("while":rest))
      after = drop ele ("while":rest)
      evenBCond = getValueBexp (parseAnd(isPar(drop 1(take(ele -1) ("while":rest)))))
      parsingAux x = parsing(drop (getValue(elemIndex x after)) after)
  in case firstElemTake after of
      "(" -> parsingAux ")" (statement ++ [ParserWhile evenBCond (parsing (take (getValue (elemIndex ")" after)) after) [])])
      _  -> parsingAux ";" (statement ++ [ParserWhile evenBCond (parsing (take (getValue (elemIndex ";" after)) after) [])])


--parsing if statements
parsing ("if":rest) statement =
  let thenValue = getValue (elemIndex "then" ("if":rest))
      elseValue = getValue (elemIndex "else" ("if":rest))
      arrayAfter = drop elseValue ("if":rest)
  in case firstElemTake arrayAfter of
    "(" -> parsing (drop (getValue (elemIndex ")" arrayAfter)) arrayAfter) (statement ++ [ParserCond (getValueBexp (parseAnd (isPar (drop 1 (take (thenValue - 1) ("if":rest)))))) (parsing (drop thenValue (take (elseValue - 1) ("if":rest))) []) (parsing (take (getValue (elemIndex ")" arrayAfter)) arrayAfter) [])])
    _   -> parsing (drop (getValue (elemIndex ";" arrayAfter)) arrayAfter) (statement ++ [ParserCond (getValueBexp (parseAnd (isPar (drop 1 (take (thenValue - 1) ("if":rest)))))) (parsing (drop thenValue (take (elseValue - 1) ("if":rest))) []) (parsing (take (getValue (elemIndex ";" arrayAfter)) arrayAfter) [])])

firstElemTake :: [String] -> String
firstElemTake [] = error"throw take first element"
firstElemTake ("(":rest) = "("
firstElemTake (a:rest) = a

getValueBexp :: Maybe (Bexp, [String]) -> Bexp
getValueBexp (Just (a, [")"])) = a
getValueBexp (Just (a, [])) = a
getValueBexp Nothing = error "throw getValueB error"

isPar :: [String] -> [String]
isPar [] = error"throw error is par"
isPar ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
isPar rest = rest

------------------------------------------------------------ PARSING OPERAÇOES ARITMETICAS ------------------------------------------------------
--SLIDES TEORICAS

parseInt :: [String] -> Maybe(Aexp,[String])   -- parsing integer literals
parseInt (n : rest) = case(readMaybe n :: Maybe Integer) of
  Just f -> Just(Num f, rest)
  Nothing -> Just (Var n, rest)
parseInt _ = Nothing


parseProdOrInt :: [String] -> Maybe(Aexp,[String]) -- parsing products
parseProdOrInt token = case parseInt token of
  Just(expr1,("*":restString1)) ->
    case parseProdOrInt restString1 of
      Just(expr2, restString2) ->
        Just(MULT expr1 expr2, restString2)
      Nothing -> Nothing
  result -> result -- can be nothing or valid


parseSumOrProdOrInt :: [String] -> Maybe(Aexp,[String]) -- parsing sums
parseSumOrProdOrInt token = case parseProdOrInt token of
  Just(expr1,"+":restString1) ->
    case parseProdOrInt restString1 of
      Just(expr2, restString2) ->
        Just(ADD expr1 expr2, restString2)
      Nothing                -> Nothing
  result -> result -- can be nothing or valid

parseIntOrParentExpr :: [String] -> Maybe(Aexp,[String]) -- parsing Parenthesised expressions
parseIntOrParentExpr ("(": rest) = case parseSumOrProdOrIntOrPar rest of
  Just(expr,(")":restString1)) -> Just (expr, restString1)
  Just _ -> Nothing -- no closing paren
  Nothing -> Nothing
parseIntOrParentExpr (n:rest) =
  case (readMaybe n :: Maybe Integer) of
    Just f -> Just (Num f, rest)
    Nothing -> Just (Var n, rest)
parseIntOrParenExpr tokens = Nothing


parseProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of
    Just (expr1,("*":restString1)) ->
      case parseProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (MULT expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result


parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp,[String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (expr1,("+":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (ADD expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,("-":restString1)) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) -> Just (SUB expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result



  ------------------------------------------------------------ PARSING BOOLEANOS ------------------------------------------------------


parseTrueOrFalse :: [String] -> Maybe(Bexp, [String])
parseTrueOrFalse("True": rest) = Just(TRUE, rest)
parseTrueOrFalse("False": rest) = Just(FALSE, rest)
parseTrueOrFalse ("(":rest) =
  case parseAnd rest of
    Just (expr, ")":restString1) -> Just (expr, restString1)
    _ -> Nothing
parseTrueOrFalse rest =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr1, op:restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) ->
          case op of
            "==" -> Just (Eq expr1 expr2, restString2)
            "<=" -> Just (LEQ expr1 expr2, restString2)
            _    -> Nothing
        _ -> Nothing
    _ -> Nothing


parseNot :: [String] -> Maybe(Bexp, [String])
parseNot("not":rest) = 
  case parseTrueOrFalse rest of 
    Just(expr1,restString1) ->
      Just(NOT expr1, restString1)
    results -> results
parseNot rest = parseTrueOrFalse rest


parseEqBool :: [String] -> Maybe(Bexp, [String])
parseEqBool rest = case parseNot rest of 
  Just(expr1, ("=":restString1)) ->
    case parseEqBool restString1 of
      Just(expr2, restString2) ->
        Just(EqBool expr1 expr2, restString2)
      Nothing -> Nothing
  results -> results


parseAnd :: [String] -> Maybe(Bexp, [String])
parseAnd rest = case parseEqBool rest of
  Just(expr1, ("and":restString1)) ->
    case parseAnd restString1 of
      Just(expr2, restString2) ->
        Just(AND expr1 expr2, restString2)
      Nothing -> Nothing
  results -> results