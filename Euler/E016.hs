{-# OPTIONS_GHC -Wall #-}

module Euler.E016 ( e016 ) where

e016 :: Integer
e016 = sum $ map (\x -> read [x]) $ show (2^(1000::Int) :: Integer)
