module DescendingOrder where


import           Data.List
descendingOrder :: Integer -> Integer
descendingOrder x =
  sum $ zipWith (\b e -> b * 10 ^ e) (sort $ digits x) [0 ..]

digits :: Integer -> [Integer]
digits 0       = []
digits numbers = y : digits x where (x, y) = numbers `divMod` 10
