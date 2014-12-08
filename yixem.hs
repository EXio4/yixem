module Main (main) where

import Yixem.Parser
import Yixem.Compiler

import System.Environment
    
main :: IO ()
main = getArgs >>= \x ->
  case x of
       [x] -> readFile x >>= \y ->
	  case parserX y of
		Right v -> putStrLn ("# file: "++x) >> putStrLn (compile v)
		Left  x -> putStrLn "Error parsing file" >> putStrLn x
       _   -> putStrLn "give it exactly one param (the file to compile)"
