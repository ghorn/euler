{-# OPTIONS_GHC -Wall #-}

module Euler.E020 ( e020 ) where

e020 :: Integer
e020 = sum $ map (\x -> read [x] :: Integer) $ show (fact 100)

fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)
