module Main (main) where

import Yixem.Parser
import Yixem.Compiler
import Yixem.Typechecker
import Yixem.Optimizer
import System.Environment
import Control.Monad
import Control.Applicative
import System.IO

main :: IO ()
main = getArgs >>= \x ->
  case x of
       [x] -> readFile x >>= \y ->
	  case parserX y of
		Right v ->
		   case typecheck v of
		      Nothing ->
			case optimizer v of
			  Right v -> putStrLn ("# file: "++x) >> putStrLn (compile v)
			  Left  e -> hPutStrLn stderr ("OPTIMIZER PHASE FAILED TO TYPECHECKER\n\n\tphase: " ++ e)
		      Just x  -> hPutStrLn stderr ("error while typechecking\n\t" ++ x)
		Left  x -> putStrLn "Error parsing file" >> putStrLn x
       _   -> putStrLn "give it exactly one param (the file to compile)"
