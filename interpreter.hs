module Interpreter where

import Container
import Data.Map.Strict as MyMap 

--- Receives the stack and returns the updated stack

add :: Stack -> Stack
add stack
  | value1 == Nothing = stack
  | Just (Number n1) <- value1,
    value2 == Nothing = stack
  | Just (Number n1) <- value1,
    Just (Number n2) <- value2 = pushToStack (Number (n1 + n2)) popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
mult :: Stack -> Stack
mult stack
  | value1 == Nothing = stack
  | Just (Number n1) <- value1,
    value2 == Nothing = stack
  | Just (Number n1) <- value1,
    Just (Number n2) <- value2 = pushToStack (Number (n1 * n2)) popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
sub :: Stack -> Stack
sub stack
  | value1 == Nothing = stack
  | Just (Number n1) <- value1,
    value2 == Nothing = stack
  | Just (Number n1) <- value1,
    Just (Number n2) <- value2 = pushToStack (Number (n1 - n2)) popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
eq :: Stack -> Stack
eq stack
  | value1 == Nothing = stack
  | Just (Number n1) <- value1,
    value2 == Nothing = stack
  | Just (Number n1) <- value1,
    Just (Number n2) <- value2 = if n1 == n2 then pushToStack TT popStack2 else pushToStack FF popStack2
  | Just TT <- value1,
    Just TT <- value2 = pushToStack TT popStack2
  | Just TT <- value1,
    Just FF <- value2 = pushToStack FF popStack2
  | Just FF <- value1,
    Just FF <- value2 = pushToStack TT popStack2
  | Just FF <- value1,
    Just TT <- value2 = pushToStack FF popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
le :: Stack -> Stack
le stack
  | value1 == Nothing = stack
  | Just (Number n1) <- value1,
    value2 == Nothing = stack
  | Just (Number n1) <- value1,
    Just (Number n2) <- value2 =
      let value = if n1 <= n2 then TT else FF
      in pushToStack value popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
and :: Stack -> Stack
and stack
  | value1 == Nothing = stack
  | Just TT <- value1,
    value2 == Nothing = stack
  | Just TT <- value1,
    Just TT <- value2 = pushToStack TT popStack2
  | Just TT <- value1,
    Just FF <- value2 = pushToStack FF popStack2
  | Just FF <- value1 = pushToStack FF popStack2
  | otherwise = stack
  where
    (value1, popStack1) = pop stack
    (value2, popStack2) = pop popStack1


-- Receives the stack and returns the updated stack
neg :: Stack -> Stack
neg stack
  | value1 == Nothing = stack
  | Just TT <- value1 = pushToStack FF popStack1
  | Just FF <- value1 = pushToStack TT popStack1
  | otherwise = stack
  where
    (value1, popStack1) = pop stack


-- Receives a StackValue and the stack and returns the updated stack
push :: Either Integer Bool -> Stack -> Stack
push (Right True)  = pushToStack TT
push (Right False) = pushToStack FF
push (Left n)      = pushToStack (Number n)


-- Receives a Key, the stack and the state and returns the updated stack
fetch :: Key -> Stack -> State -> Stack
fetch key stack state
  | Just value <- MyMap.lookup key state = pushToStack value stack
  | otherwise = stack

-- Receives a Key, the stack and the state and returns the updated stack and state
store :: Key -> Stack -> State -> (Stack, State)
store key stack state
  | Just value <- val = (stackAfterPop, insertIntoState key value state)
  | otherwise = (stackAfterPop, state)
  where
    (val, stackAfterPop) = pop stack


-- Receives 2 code flows and the stack and returns one of the code flows and the updated stack
branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 stack
  | Just value <- val = (if value == TT then c1 else c2, stackAfterPop)
  | otherwise = (c1, stackAfterPop)
  where
    (val, stackAfterPop) = pop stack

-- Receives 2 code flows, the stack and the state and returns the remaining code flow, the updated stack and updated state
loop :: Code -> Code -> Code
loop c1 c2 = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]]

-- Dummy function that receives the stack and state and returns them both
noop :: Stack -> State -> (Stack, State)
noop stack state = (stack, state)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (current:rest, stack, state) =
    case current of
        Push n -> run (rest, push (Left n) stack, state)
        Add -> run (rest, add stack, state)
        Mult -> run (rest, mult stack, state)
        Sub -> run (rest, sub stack, state)
        Tru -> run (rest, push (Right True) stack, state)
        Fals -> run (rest, push (Right False) stack, state)
        Equ -> run (rest, eq stack, state)
        Le -> run (rest, le stack, state)
        And -> run (rest, Interpreter.and stack, state)
        Neg -> run (rest, Interpreter.neg stack, state)
        Fetch key -> run (rest, fetch key stack state, state)
        Store key -> let (newStack, newState) = store key stack state in run (rest, newStack, newState)
        Noop -> let (newStack, newState) = noop stack state in run (rest, newStack, newState)
        Branch code1 code2 -> let (rest, newStack) = branch code1 code2 stack in run (rest, newStack, state)
        Loop code1 code2 -> run (loop code1 code2, stack, state)