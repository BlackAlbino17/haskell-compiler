# Haskell-Compiler

## Group Information

**Group Name:** T04_G02

**Student Numbers and Names:**
- up202108823: João Pedro Oliveira Sequeira
- up202008790: Tiago Rocha Silveira Pires

## Contribution

- João Pedro Oliveira Sequeira - 50%
- Tiago Rocha Silveira Pires - 50%

## Strategy and Decision-Making

- We decided to use a modular approach by separating the data structures, Interpreter, and compiler into distinct modules (`container.hs`, `Interpreter.hs`, `parser.hs`).
- The `Container` module defines the instructions, stack, and state types.
- The `Interpreter` module contains functions to interpret instructions, manipulate the stack and state, and run the code.
- The `Tests` module provides a set of tests for the Interpreter.
- The `Parser` module is currently a placeholder for future development.

# Container Module

The `Container` module provides a simple implementation of a stack-based virtual machine. It defines data structures for code, stack, and state, along with functions to operate on them.

## Instructions

The `Inst` data type represents the instructions that can be executed in the virtual machine. Instructions include operations like arithmetic, logic, and control flow. The `Code` type is a list of instructions, forming a program.

### Instructions:
- `Push Integer`: Push an integer onto the stack.
- `Add`, `Mult`, `Sub`, `Tru`, `Fals`, `Equ`, `Le`, `And`, `Neg`: Arithmetic and logical operations.
- `Fetch String`: Load the value of a variable from the state onto the stack.
- `Store String`: Store a value from the stack into a variable in the state.
- `Noop`: No-operation.
- `Branch Code Code`: Conditional branch.
- `Loop Code Code`: Loop construct.

## Stack

The `Stack` data type is a stack of `StackValue`s, where a `StackValue` can be either a number, `True` (TT), or `False` (FF).

### Stack Functions:
- `pushToStack`: Push a value onto the stack.
- `pop`: Pop a value from the stack.
- `createEmptyStack`: Create an empty stack.
- `stack2Str`: Convert a stack to a string.

## State

The `State` data type is a key-value store implemented using `Data.Map.Strict`. Keys are strings, and values are `StackValue`s.

### State Functions:
- `insertIntoState`: Insert a key-value pair into the state.
- `createEmptyState`: Create an empty state.
- `state2Str`: Convert a state to a string.

## Code Functions
- `popCode`: Pop an instruction from the code.

# Interpreter Module

The `Interpreter` module implements an interpreter for a simple stack-based programming language. It utilizes the data structures and functions provided by the `Container` module.

## Arithmetic and Logic Operations

### `add`
This function performs addition. It pops two values from the stack, adds them, and pushes the result back onto the stack. If there are not enough values on the stack or if the values are not numbers, it throws a run-time error.

### `mult`
Similar to `add`, this function performs multiplication.

### `sub`
Handles subtraction by popping two values, subtracting them, and pushing the result back onto the stack.

### `eq`
Implements equality comparison. It handles cases where the values on the stack can be numbers or boolean values (True or False). It pushes True (TT) onto the stack if the values are equal; otherwise, it pushes False (FF).

### `le`
Performs less than or equal to comparison for numbers on the stack. It pushes True (TT) if the first value is less than or equal to the second; otherwise, it pushes False (FF).

### `and`
Implements logical AND. It pushes True (TT) onto the stack if both values on the stack are True; otherwise, it pushes False (FF).

### `neg`
Implements logical negation. It flips the boolean value on the top of the stack.

## Stack Manipulation

### `push`
Pushes a value onto the stack. It takes an `Either Integer Bool` parameter, where `Left n` represents an integer value, and `Right True` or `Right False` represents boolean values.

### `fetch`
Loads the value of a variable from the state onto the stack. It takes a key, searches for it in the state, and pushes the corresponding value onto the stack.

### `store`
Stores a value from the stack into a variable in the state. It pops a value from the stack, and if the key exists in the state, it updates the state with the new value.

## Control Flow

### `branch`
Conditional branch based on the top of the stack. It takes two code flows and the current stack. Depending on whether the top of the stack is True (TT) or False (FF), it selects the corresponding code flow and updates the stack.

### `loop`
Implements a loop construct. It takes two code flows and creates a loop structure using the `Branch` and `Noop` instructions.

## Utility Functions

### `noop`
A dummy function that receives the stack and state and returns them unchanged. It serves as a no-operation placeholder.

### `run`
The main function for executing code. It takes a triple `(Code, Stack, State)` and iteratively processes instructions until the code is empty. It updates the stack and state based on the executed instructions.

In summary, the `Interpreter` module provides a set of functions to perform arithmetic and logic operations, manipulate the stack, handle control flow, and execute a sequence of instructions. It builds on the data structures and functions defined in the `Container` module, creating a flexible and extensible interpreter for a stack-based language.

# Tests Module

The `Tests` module includes tests for the `Assembler`, `Interpreter`, and `Parser` modules. It defines a set of functions to test the functionality of the stack-based programming language, from assembly to parsing.

## Assembler Tests

### `testAssembler`
This function takes a code and returns a tuple containing the string representations of the stack and state after running the code. It uses the `run` function from the `Interpreter` module to execute the code.

### `runAllAssemblerTests`
This IO action runs a series of tests using `testAssembler` and prints the results. The tests include various code scenarios to ensure the correct functioning of the assembler.

# Parser (Placeholder)

- The `Parser` module is currently incomplete in a way that it doesnt meet the criteria for the parser tests on the while and if statements.
- This was do to problems we had building the functions regarding those statements and some debug issues we had on which we couldnt unfortunetely
 complete this functions with success. The tests that dont pass are commented on tests.hs

- Regarding the parser.hs file we started by defining data and types that we thought would be used on this second stage of the project 

```hs
data Aexp = ADD Aexp Aexp
          | SUB Aexp Aexp
          | MULT Aexp Aexp
          | Num Integer
          | Var String
          deriving (Eq, Show)
-- verified
data Bexp = TRUE
          | FALSE
          | NOT Bexp
          | AND Bexp Bexp
          | LEQ Aexp Aexp
          | Eq Aexp Aexp -- expressoes com =
          | EqBool Bexp Bexp -- expressoes com ==
          deriving (Eq, Show)
-- verified

data Stm = Assign String Aexp
         | Seq Stm Stm
         | IfThenElse Bexp [Stm] [Stm]
         | While Bexp [Stm]
         deriving (Eq, Show)

-- verified
type Program = [Stm]
```

We then proceded to implement the compiler that would be responsible for receiving an argument and return the Code (list with program instructions).
We created compilers for both arithmetic expressions and boolean expressions, 'compA' and 'compB' respectively
```hs
compile :: Program -> Code
compile [] = []
compile (stmt:rest) = case stmt of
  Assign var expr -> compA expr ++ [Store var] ++ compile rest
  Seq stmt1 stmt2 -> compile [stmt1] ++ compile [stmt2] ++ compile rest
  IfThenElse cond thenStmt elseStmt ->
    compB cond ++ [Branch (compile thenStmt) (compile elseStmt)] ++ compile rest
  While cond body ->
    [Branch (compB cond ++ [Branch (compile body ++ [Branch (compile [While cond body]) [Noop]]) [Noop]]) [Noop]] ++ compile rest

```
This is what the main compile function looks like! It processes each statement in the program based on its type.
For the assign case we compute the expression value and store it in the corresponding variable; for sequence statements we went to recursion to compile each sub-statement and concatenate the result; On the If Else case we generate code to evaluate the condition and branch accordingly; For the while statements we do a loop structure using conditional branches and no operation instructions.


Regarding the parser itself we tried to follow the requirements of the project on its definition. We used lexer as instructed on the theoretical classes to help us separate the content of the instructions and store it on a list of strings.
We also implemented several parsing expressions based on the theoretical slides that helped us parse the different arithmetic and boolean expressions.

To help the parser function we created a parsing function to aid passing the string list to lexer. Once it receives the list it will iterate throught it while trying to match each case of the parsing regarding parenthesis, if and else statements, while statements and assignment statements. (If and else statements are not working as expected).

# How to Run

Follow these steps to run the project:

1. **Download the Project:**
   - Download or clone the project repository to your local machine.

2. **Navigate to the `src` Folder:**
   - Open a terminal or command prompt.
   - Navigate to the `src` folder of the project using the `cd` command.

3. **Run GHCi (Glasgow Haskell Compiler Interactive):**
   - Type the following command to start GHCi:
     ```bash
     ghci
     ```

4. **Load the Main Module:**
   - Once GHCi is running, load the main module using the following command:
     ```haskell
     :l main.hs