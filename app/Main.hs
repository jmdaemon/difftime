module Main where

import DiffTimeLib
import System.Environment
import System.Exit

import Options.Applicative
import Data.Monoid ((<>))

data Sample = Sample
  { t1 :: String
  , t2 :: String }

--replicateString :: Sample -> IO ()
--replicateString (Sample string n flip) = 
    --do 
      --if not flip then putStrLn repstring else putStrLn $ reverse repstring
          --where repstring = foldr (++) "" $ replicate n string
--parse :: Sample -> IO ()
--parse :: Sample -> IO () -> String
--parse :: Sample -> IO() -> String
parse :: Sample -> IO()
parse (Sample t1 t2) =
        putStrLn $ toDiffTime t1 t2
        --putStrLn t1
        --putStrLn (read t1 :: String)
        --putStrLn (read t1 :: String) (read t2 :: String)
        --putStrLn (diffTime (read t1 :: String) (read t2 :: String))
        --putStrLn $ diffTime do(t1 t2)
        --putStrLn $ do (diffTime <- t1 t2)
    --putStrLn $ t1 t2 >>= diffTime
    --putStrLn $ diffTime <- t1 t2
    --diffTime >>= t1 t2
    --do
        --diffTime $ replicate <- t1 t2
        --replicate t1 t2 >>= diffTime t1 t2
        --diffTime $ replicate t1 t2
        --diffTime $ putStrLn repstring
        --diffTime repstring
            --where repstring = replicate t1 t2
            --where repstring = foldr (++) "" $ replicate t1 t2

    --do
        --if not flip then putStrLn repstring
                    --else putStrLn $ reverse repstring
          --where repstring = foldr (++) "" $ replicate n string
    --putStr $ toDiffTime t1 t2
    --toDiffTime t1 t2
    --putStrLn $ toDiffTime t1 t2

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

--main :: IO ()

--main = someFunc

--main = do
    --args <- getArgs
    --putStrLn $ head args

--main = do
    --args <- getArgs
    --putStrLn toDiffTime

-- diffTime "10:30pm" "9:00pm"
toDiffTime :: String -> String -> String
--toDiffTime :: [String] -> String
--toDiffTime [s1, s2] = 
--toDiffTime s1 s2 = 
    ----let t1 = head s1
        ----t2 = concat (tail s2)
    --let t1 = read (head s1) :: String
        --t2 = read (concat (tail s2)) :: String
     --in diffTime t1 t2
toDiffTime s1 s2 = 
    --let t1 = head s1
        --t2 = concat (tail s2)
        diffTime s1 s2

--usage   = putStrLn "Usage: difftime [TIME] [TIME]"
--version = putStrLn "difftime v0.1.0"
--exit    = exitWith ExitSuccess
--die     = exitWith (ExitFailure 1)
