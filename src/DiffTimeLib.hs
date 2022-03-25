module DiffTimeLib
    (diffTimeHourMin
    ) where

import Text.Printf
import Data.List

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

data Interval = Interval Int Int Int

diff :: Int -> Int -> Int
diff final initial = abs(final - initial)

diffInterval :: Interval -> Interval -> [Int]
diffInterval (Interval h_f m_f s_f) (Interval h_i m_i s_i) = [diff h_f h_i, diff m_f m_i, diff s_f s_i]

sumTimeDiff :: [Int] -> [Int] -> [Int]
sumTimeDiff [h1,m1,s1] [h2,m2,s2] = [h1 + h2, m1 + m2, s1 + s2]

showTimeDiff :: [Int] -> String
showTimeDiff [h,m,s] = printf "%d hours %d mins %d secs" h m s

toInterval :: Int -> Int -> Int -> Interval
toInterval h m s = Interval h m s

diffTimeHourMin :: String -> String -> String
diffTimeHourMin t1 t2 =
    let [h1,m1] = toTimeList t1
        [h2,m2] = toTimeList t2
    in showTimeDiff (diffInterval (toInterval h2 m2 0) (toInterval h1 m1 0))

    --let [h1,m1,s1] = toTimeList t1
        --[h2,m2,s2] = toTimeList t2
    --in showTimeDiff (diffInterval (toInterval h2 m2 s2) (toInterval h1 m1 s1))

-- Pad adds zeros to the time
padTime :: String -> String -> [Int]
padTime time pad = 
    -- Creates HH:MM:00
    if pad == "hm"
       then
       let hm = toTimeList time
        in hm ++ [0]
    -- Creates 00:MM:SS
    else
        let ms = toTimeList time
         in 0:ms

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

toTimeList :: String -> [Int]
toTimeList string = map (read :: String->Int) (splitColon string)
