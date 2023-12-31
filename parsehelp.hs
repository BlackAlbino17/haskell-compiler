-- Part 2

-- Arithmetic expressions
data Aexp
  = Num Integer          -- Represents a numeric constant
  | Var String       -- Represents a variable
  | Addition Aexp Aexp    -- Represents addition of two expressions
  | Subtraction Aexp Aexp    -- Represents subtraction of two expressions
  | Mul Aexp Aexp    -- Represents multiplication of two expressions
  deriving Show

-- Boolean expressions
data Bexp
  = BTrue            -- Represents the boolean constant true
  | BFalse           -- Represents the boolean constant false
  | Eq Aexp Aexp     -- Represents equality comparison of two expressions
  | Leq Aexp Aexp    -- Represents less than or equal comparison of two expressions
  | LogicalAnd Bexp Bexp    -- Represents logical AND of two boolean expressions
  | EqBool Bexp Bexp     -- Represents logical OR of two boolean expressions
  | Not Bexp         -- Represents logical NOT of a boolean expression
  deriving Show

-- Statements
data Stm
  = Assign String Aexp
  | Cond Bexp [Stm] [Stm]
  | While Bexp [Stm]
  deriving Show

type Program = [Stm]




parseHelper ("while":rest) statm =
  let doElement = getJust (elemIndex "do" ("while":rest))
      missingPart = drop doElement ("while":rest)
      cond = getJustBexp (parseAndandEqBool (isEven (drop 1 (take (doElement - 1) ("while":rest)))))
      parseHelperDrop symbol = parseHelper (drop (getJust (elemIndex symbol missingPart)) missingPart)
  in case takeFirstEleList missingPart of
       "(" -> parseHelperDrop ")" (statm ++ [While cond (parseHelper (take (getJust (elemIndex ")" missingPart)) missingPart) [])])
       _  -> parseHelperDrop ";" (statm ++ [While cond (parseHelper (take (getJust (elemIndex ";" missingPart)) missingPart) [])])

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





takeFirstEleList :: [String] -> String
takeFirstEleList [] = error "Empty list"
takeFirstEleList ("(":_) = "("
takeFirstEleList (a:_) = a


