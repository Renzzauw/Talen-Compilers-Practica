module Main where

import System.Environment
import System.FilePath

import ParseLib.Abstract.Derived
import Prelude hiding ((<*),(*>),(<$))

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM
import CSharpCode

-- Runs a parser
start :: Parser s a -> [s] -> a
start p = fst . head . filter (null . snd) . parse p

-- Entry point of the program
main :: IO ()
main = do
         -- get command line arguments
         args  <- getArgs
         -- compute a list of input an output files
         files <- case args of
                    []  ->  do
                              putStrLn "no argument given; assuming example.cs"
                              return [("../example.cs", "../example.ssm")]
                    xs  ->  return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
         -- translate each of the files
         mapM_ processFile files

-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
    _ <- getChar
    return ()
  where process = formatCode
                . errorCheck
                . foldCSharp codeAlgebra
                . start (pClass <* eof)
                . start lexicalScanner . start commentScanner

-- BONUS EXERCISE --                
-- Looks through all the instructions to check for errors                
-- Returns the original code if no error occurs                
errorCheck :: Code -> Code
-- Divide by zero exception
errorCheck (LDC 0:DIV:xs) = error ("Error: Divide by zero exception!") 
errorCheck (LDC 0:MOD:xs) = error ("Error: Divide by zero exception!")
errorCheck (x:[])         = [x]
errorCheck (x:y:xs)       = x : errorCheck (y:xs)