module Main where

import Container
import Interpreter
import Parser
import Tests( runAllAssemblerTests, testParser)



-- main will just run all the tests, the function that processes the Code, Stack and State is the run function
main :: IO ()
main = do
    let inputProgram = "x := 5;x := x - 1;"
    putStrLn "Input Program:"
    putStrLn inputProgram

    let parsedProgram = parse inputProgram
    putStrLn "\nParsed Program:"
    print parsedProgram

    print $ testParser "x := 5; x := x - 1;"


