module Main (main) where

import Yixem.Parser
import Yixem.Compiler
import Yixem.Typechecker
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
		      Nothing -> putStrLn ("# file: "++x) >> putStrLn (compile v)
		      Just x  -> hPutStrLn stderr ("error while typechecking\n\t" ++ x)
		Left  x -> putStrLn "Error parsing file" >> putStrLn x
       _   -> putStrLn "give it exactly one param (the file to compile)"
