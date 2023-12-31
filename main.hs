module Main where

import Container
import Interpreter
import Parser
import Tests


-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
   -- runAllAssemblerTests
    runAllParserTests
