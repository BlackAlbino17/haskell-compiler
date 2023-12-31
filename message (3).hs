





















parseHelper ("if":rest) statm =
    let thenElement = getJust (elemIndex "then" ("if":rest))
        elseElement = getJust (elemIndex "else" ("if":rest))
        missingPart = drop elseElement ("if":rest)
    in case takeFirstEleList missingPart of
        "(" ->
            let condition = parseAndandEqBool (isEven (drop 1 (take (thenElement - 1) ("if":rest))))
                thenBranch = parseHelper (drop thenElement (take (elseElement - 1) ("if":rest))) []
                elseBranch = parseHelper (take (getJust (elemIndex ")" missingPart)) missingPart) []
            in parseHelper (drop (getJust (elemIndex ")" missingPart)) missingPart) (statm ++ [Cond (getJustBexp condition) thenBranch elseBranch])
        _ ->
            let condition = parseAndandEqBool (isEven (drop 1 (take (thenElement - 1) ("if":rest))))
                thenBranch = parseHelper (drop thenElement (take (elseElement - 1) ("if":rest))) []
                elseBranch = parseHelper (take (getJust (elemIndex ";" missingPart)) missingPart) []
            in parseHelper (drop (getJust (elemIndex ";" missingPart)) missingPart) (statm ++ [Cond (getJustBexp condition) thenBranch elseBranch])




getJustBexp :: Maybe (Bexp, [String]) -> Bexp
getJustBexp (Just (a, [")"])) = a
getJustBexp (Just (a, [])) = a
getJustBexp Nothing = error "getJustBexp failure"

isEven :: [String] -> [String]
isEven [] = error "Empty list"
isEven ("(":rest) = drop 1 (take (length ("(":rest)) ("(":rest))
isEven rest = rest

takeFirstEleList :: [String] -> String
takeFirstEleList [] = error "Empty list"
takeFirstEleList ("(":_) = "("
takeFirstEleList (a:_) = a


------------------------------- Parse for Arithmetics --------------------------------





parseProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseProdOrIntOrPar rest =
  case parseIntOrParentExpr rest of
    Just (expr1,"*":restString1) ->
      case parseProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (Mul expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseSumOrProdOrIntOrPar :: [String] -> Maybe (Aexp, [String])
parseSumOrProdOrIntOrPar rest =
  case parseProdOrIntOrPar rest of
    Just (expr1,"-":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (Subtraction expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"+":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2, restString2) -> Just (Addition expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result


------------------------------- Parse for Comparators --------------------------------

parseArithOrParOrFalseOrTrueOrEqOrLess :: [String] -> Maybe (Bexp, [String])
parseArithOrParOrFalseOrTrueOrEqOrLess ("True":rest) = Just (BTrue,rest)
parseArithOrParOrFalseOrTrueOrEqOrLess ("False":rest) = Just (BFalse,rest)
parseArithOrParOrFalseOrTrueOrEqOrLess ("(":rest) =
  case parseAndandEqBool rest of
    Just (expr,")":restString1) -> Just (expr,restString1)
    Just _ -> Nothing
    Nothing -> Nothing
parseArithOrParOrFalseOrTrueOrEqOrLess rest =
  case parseSumOrProdOrIntOrPar rest of
    Just (expr1,"==":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (Eq expr1 expr2, restString2)
        Nothing -> Nothing
    Just (expr1,"<=":restString1) ->
      case parseSumOrProdOrIntOrPar restString1 of
        Just (expr2,restString2) ->
          Just (Leq expr1 expr2, restString2)
        Nothing -> Nothing
    result -> Nothing

parseNegAndEqAndLess :: [String] -> Maybe(Bexp, [String])
parseNegAndEqAndLess ("not":rest) =
    case parseArithOrParOrFalseOrTrueOrEqOrLess rest of
      Just (expr1,restString1) ->
        Just (Not expr1,restString1)
      result -> result
parseNegAndEqAndLess rest = parseArithOrParOrFalseOrTrueOrEqOrLess rest

parseBoolEqAndNeg :: [String] -> Maybe(Bexp, [String])
parseBoolEqAndNeg rest =
  case parseNegAndEqAndLess rest of
    Just (expr1, "=":restString1) ->
      case parseBoolEqAndNeg restString1 of
        Just (expr2, restString2) ->
          Just (EqBool expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

parseAndandEqBool :: [String] -> Maybe(Bexp, [String])
parseAndandEqBool rest =
  case parseBoolEqAndNeg rest of
    Just (expr1, "and":restString1) ->
      case parseAndandEqBool restString1 of
        Just (expr2, restString2) ->
          Just (LogicalAnd expr1 expr2, restString2)
        Nothing -> Nothing
    result -> result

-------------------------------------------------------------------------------------


getJust :: Num a => Maybe a -> a
getJust (Just a) = a + 1
getJust Nothing = error "Value not found"

lexer :: String -> [String]
lexer "" = []
lexer input = go input []
  where
    go :: String -> [String] -> [String]
    go [] acc = reverse acc
    go (c:cs) acc
      | isSpace c = go cs acc
      | isDigit c = let (token, rest) = span isDigit (c:cs)
                    in go rest (token : acc)
      | c == '('   = go cs ("(" : acc)
      | c == ')'   = go cs (")" : acc)
      | c == ';'   = go cs (";" : acc)
      | c == '+'   = go cs ("+" : acc)
      | c == '*'   = go cs ("*" : acc)
      | otherwise = let (token, rest) = span (\x -> not (isSpace x || isDigit x || x `elem` "();+*")) (c:cs)
                    in go rest (token : acc)

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:  
-- testParser "x := 5; x := x - 1;" == ("","x=4")                                                         
-- testParser "x := 0 - 2;" == ("","x=-2")                                                                
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")                  
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")                 
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")                    
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")      
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68") 
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")                 
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")                          
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")                        
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")                    
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")   
