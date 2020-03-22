module Divisor where


divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = if isPrime a
  then Left $ show a ++ " is prime"
  else Right [ n | n <- [2 .. a - 1], a `mod` n == 0 ]


isPrime :: (Show a, Integral a) => a -> Bool
isPrime x = case length primes of
  1 -> True
  _ -> False
  where primes = [ n | n <- [1 .. x - 1], x `mod` n == 0 ]
