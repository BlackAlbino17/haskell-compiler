module Main where

import Container
import Evaluator
--import Parser
import Tests( runAllAssemblerTests)


-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
    runAllAssemblerTests
    --runAllParserTestsnot(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")