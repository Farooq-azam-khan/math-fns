module Choose where

choose :: Int -> Int -> Int
choose n k 
  | k <= 0 = 1
  | k > n = 0 
  | otherwise = (choose (n-1) (k-1)) + (choose (n-1) k)
