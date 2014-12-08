module Main (main) where

import Yixem.Parser
import Yixem.Compiler
import Yixem.Typechecker
import System.Environment
import Control.Monad
import Control.Applicative

main :: IO ()
main = getArgs >>= \x ->
  case x of
       [x] -> readFile x >>= \y ->
	  case parserX y of
		Right v -> typecheck v >>= \y ->
		    when y $ do
			  putStrLn ("# file: "++x)
			  putStrLn (compile v)
		Left  x -> putStrLn "Error parsing file" >> putStrLn x
       _   -> putStrLn "give it exactly one param (the file to compile)"
