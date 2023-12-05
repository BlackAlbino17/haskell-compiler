-- State.hs

module State (StateElement, State, emptyState) where

-- StateElement type to represent variable bindings
type StateInt = (String,Integer)
type StateBool = (String,Bool)
data StateElement = StateInt | StateBool deriving (Show)

-- State type to represent a list of StateElements
type State = [StateElement]

emptyState :: State
emptyState = []


{-
, lookupVar, updateVar
-- Function to create an empty state


-- Function to look up a variable in the state
lookupVar :: String -> State -> Maybe StateElement
lookupVar varName state = lookup varName [(name, value) | (IntVar name value) <- state] <|>
                         lookup varName [(name, value) | (BoolVar name value) <- state]

-- Function to update a variable in the state
updateVar :: StateElement -> State -> State
updateVar element state =
  let updatedState = filter (\e -> case e of { IntVar name _ -> name /= varName; BoolVar name _ -> name /= varName }) state
  in element : updatedState
  where
    varName = case element of
      IntVar name _ -> name
      BoolVar name _ -> name
-}