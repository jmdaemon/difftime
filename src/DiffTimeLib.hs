module DiffTimeLib
    (
    diffTime
    ) where

import Text.Printf
import Data.List

-- | Splits a string into a list using any delimeter
-- | Example: split "-" string
split :: String -> String -> [String]
split _ "" = []
split delim str =
  split' "" str []
  where
    dl = length delim

    split' :: String -> String -> [String] -> [String]
    split' h t f
      | dl > length t = f ++ [h ++ t]
      | delim == take dl t = split' "" (drop dl t) (f ++ [h])
      | otherwise = split' (h ++ take 1 t) (drop 1 t) f

splitColon string = split ":" string
splitHypen string = split "-" string


-- | Calculate the difference between two integers
diff :: (Num a) => a -> a -> a
diff final initial = abs(final - initial)

-- | Converts a list of strings into a list of integers
sltoil :: [String] -> [Int]
sltoil stringList = map (read :: String->Int) stringList

-- | Converts a string into a list of integers
stoiList :: String -> [Int]
stoiList string = sltoil (splitColon string)

-- | Removes a given suffix from a string
removeSuffix :: String -> String -> String
removeSuffix delim string = concat (take 1 (split delim string))

-- | Data Types

-- | Time | --
data Time = Time
    { hours :: Int
    , minutes :: Int
    , seconds :: Int
    }

data TwelveHour = TwelveHour
    { time :: Time
    , meridiem :: String
    }

-- | Interval | --
data Interval = Interval { initial :: Time
                         , final :: Time
                         }

-- | Time Data Functions | --

-- | Sum two times together
sumTime :: Time -> Time -> Time
sumTime (Time h1 m1 s1) (Time h2 m2 s2) = Time (h1 + h2)  (m1 + m2)  (s1 + s2)

-- | Format the time into a human readable string
showTime :: Time -> String
showTime (Time h m s) = printf "%d hours %d mins %d secs" h m s

-- | Appends or prepends zeroes to some time
padTime :: String -> String -> Time
padTime time pad = 
    -- Creates HH:MM:00
    if pad == "hm"
       then
       let [h,m] = stoiList time
        in Time h m 0
    -- Creates 00:MM:SS
    else -- Assume our time is in minutes and seconds
        let [m,s] = stoiList time
         in Time 0 m s

-- | Converts a time from 12 Hours to 24 Hours
-- | E.g from12To24Hour "1:30pm" -> "13:30"
from12To24Hour :: String -> Time
from12To24Hour string =
    let [h,m,s] = sltoil(splitColon (removeSuffix "pm" string))
     in Time h m s
         
{-  Converts a time string to Time
    Note that this function also implicitly converts the
    time from 12 hour to 24 hours for easier parsing.
    E.g
        stotime "10:30pm" -> Time { 10 30 00 }
        stotime "1:30pm" -> Time { 13 30 00 }
-}
stotime :: String -> Time
stotime string
  | isSubsequenceOf "am" string =
      let [h,m,s] = stoiList (removeSuffix "am" string)
       in Time h m s
  | isSubsequenceOf "pm" string = from12To24Hour string
  | otherwise = -- Assume its already in 24 hour format
      let [h,m,s] = stoiList string
       in Time h m s

-- | Interval Data Functions | --

-- | Calculate the difference in time over a time interval
diffInterval :: Interval -> Time
diffInterval (Interval t_f t_i) = Time (hours t_f - hours t_i) (minutes t_f - minutes t_i) (seconds t_f - seconds t_i)

-- | Calculates the difference between two times and show their difference
diffTime :: String -> String -> String
diffTime t1 t2 =
    let [h1,m1] = mkTimeList t1
        [h2,m2] = mkTimeList t2
    in showTime (diffInterval (mkTime h2 m2 0) (mkTime h1 m1 0))

    --let [h1,m1,s1] = mkTimeList t1
        --[h2,m2,s2] = mkTimeList t2
    --in showTime (diffInterval (mkTime h2 m2 s2) (mkTime h1 m1 s1))
