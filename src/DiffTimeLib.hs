module DiffTimeLib
    (
    diffTimeHourMin
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
diff :: Int -> Int -> Int
diff final initial = abs(final - initial)

-- | Data Types

-- | Time | --
data Time = Time { hours :: Int
                 , minutes :: Int
                 , seconds :: Int
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
       let [h,m] = map (read :: String->Int) (splitColon time)
        in Time h m 0
    -- Creates 00:MM:SS
    else -- Assume our time is in minutes and seconds
        let [m,s] = map (read :: String->Int) (splitColon time)
         in Time 0 m s

-- | Interval Data Functions | --

-- | Calculate the difference in time over a time interval
diffInterval :: Interval -> Time
diffInterval (Interval t_f t_i) = Time (hours t_f - hours t_i) (minutes t_f - minutes t_i) (seconds t_f - seconds t_i)

-- | Calculates the difference between two times and show their difference
diffTimeHourMin :: String -> String -> String
diffTimeHourMin t1 t2 =
    let [h1,m1] = mkTimeList t1
        [h2,m2] = mkTimeList t2
    in showTime (diffInterval (mkTime h2 m2 0) (mkTime h1 m1 0))

    --let [h1,m1,s1] = mkTimeList t1
        --[h2,m2,s2] = mkTimeList t2
    --in showTime (diffInterval (mkTime h2 m2 s2) (mkTime h1 m1 s1))

-- Pad adds zeros to the time


convert12Hour :: String -> [Int]
convert12Hour string =
    --let time = splitColon (concat (take 1 (split "pm" string)))
     --in [12 + read (head time) :: Int, read (concat (tail time)) :: Int]
    let [h, m, s] = splitColon (concat (take 1 (split "pm" string)))
     in [12 + read h :: Int, read m :: Int, read s :: Int]

to24Hour :: String -> String
to24Hour string
  | isSubsequenceOf "am" string = concat (take 1 (split "am" string))
  | isSubsequenceOf "pm" string = intercalate ":" (map show (convert12Hour string))
  -- isSubsequenceOf "pm" string = 
      --let convertedString = intercalate ":" (map show (convert12Hour string))
      --in "0" ++ convertedString
       --in convertedString ++ "0"
  | otherwise = string -- Assume its already in 24 hour format

mkTimeList :: String -> [Int]
mkTimeList string = map (read :: String->Int) (splitColon string)
