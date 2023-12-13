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

### Data Structures

- We decided to use a modular approach by separating the data structures, Interpreter, and compiler into distinct modules (`container.hs`, `Interpreter.hs`, `parser.hs`).
- The `Container` module defines the instructions, stack, and state types.
- The `Interpreter` module contains functions to interpret instructions, manipulate the stack and state, and run the code.
- The `Tests` module provides a set of tests for the Interpreter.
- The `Parser` module is currently a placeholder for future development.

### Interpreter

- The Interpreter (`Interpreter.hs`) provides functions for each instruction type, such as arithmetic and logical operations, stack manipulation, and state manipulation.
- We followed a pattern of using pattern matching to handle different cases for instructions.
- The `run` function recursively processes instructions until the code is empty.

### Tests

- The `Tests` module includes a set of tests for the Interpreter (`runAllAssemblerTests`).
- Tests cover various scenarios, including arithmetic operations, logical operations, stack manipulation, and state manipulation.

### Parser (Placeholder)

- The `Parser` module is currently incomplete and serves as a placeholder for future development.
- It includes commented-out code for defining types (`Aexp`, `Bexp`, `Stm`, and `Program`) and compiler functions (`compA`, `compB`, `compile`, `parse`).

## How to Run

- Provide instructions on how to run the code or tests, dependencies, and any other relevant information.

