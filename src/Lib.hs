module Lib
    ( someFunc
    ) where

import Text.Printf

someFunc :: IO ()
someFunc = putStrLn "someFunc"


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

splitColon string = split ":"
splitHypen string = split "-"

splitAM string = split "am"
splitPM string = split "pm"

data Interval = Interval Int Int Int

diff :: Int -> Int -> Int
diff final initial = final - initial

diffInterval :: Interval -> Interval -> [Int]
diffInterval (Interval h_f m_f s_f) (Interval h_i m_i s_i) = [diff h_f h_i, diff m_f m_i, diff s_f s_i]
