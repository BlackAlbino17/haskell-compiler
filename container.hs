-- Import necessary modules
module Container where

import qualified Data.Map.Strict as MyMap
import Data.List (intercalate, sort)
import Data.Map (toList)

-- Define the Instructions data type and the Code type composed of Instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Function to pop an instruction from the code
popCode :: Code -> (Maybe Inst, Code)
popCode []     = (Nothing, [])
popCode (x:xs) = (Just x, xs)

-- Define the Stack data type and functions to operate on Stacks
data StackValue = Number Integer | TT | FF deriving (Show, Eq)
type Stack = [StackValue]

-- Function to convert a StackValue to a String
stackValueToString :: StackValue -> String
stackValueToString (Number i) = show i
stackValueToString TT = "True"
stackValueToString FF = "False"

-- Function to push a value onto the stack
pushToStack :: StackValue -> Stack -> Stack
pushToStack x stk = x : stk

-- Function to pop a value from the stack
pop :: Stack -> (Maybe StackValue, Stack)
pop []     = (Nothing, [])
pop (x:xs) = (Just x, xs)

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Convert a stack to a string
stack2Str :: Stack -> String
stack2Str stk = intercalate "," (map stackValueToString stk)

-- Define the State data type (Storage) and functions to operate on it
type Key = String
type Value = StackValue

type State = MyMap.Map Key Value

-- Function to convert a key-value pair to a string
tupleToString :: (String, StackValue) -> String
tupleToString (key, Number i) = key ++ "=" ++ show i
tupleToString (key, TT) = key ++ "=True"
tupleToString (key, FF) = key ++ "=False"

-- Create an empty state
createEmptyState :: State
createEmptyState = MyMap.empty

-- Insert a key-value pair into the state
insertIntoState :: Key -> Value -> State -> State
insertIntoState k v state = MyMap.insert k v state

-- Convert a state to a string
state2Str :: State -> String
state2Str state = intercalate "," (sort(map tupleToString (toList state)))
