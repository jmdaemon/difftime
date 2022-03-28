module Main where

import DiffTimeLib
import System.Environment
import System.Exit

import Options.Applicative
import Data.Monoid ((<>))

data Sample = Sample
  { t1 :: String
  , t2 :: String }

-- We cannot directly return a String from an impure function
-- so we just print out the result to stdout
parse :: Sample -> IO()
parse (Sample t1 t2) =
        putStrLn $ toDiffTime t1 t2

sample :: Parser Sample
sample = Sample
     <$> argument str
          ( metavar "INITIAL_TIME"
         <> help "The initial time" )
     <*> argument str
          ( metavar "FINAL_TIME"
         <> help "The final time" )

main :: IO ()
main = execParser opts >>= parse
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "Calculate the difference of a time interval"
     <> header "difftime - Calculate time differences of intervals" )

toDiffTime :: String -> String -> String
toDiffTime s1 s2 = 
        diffTime s1 s2
