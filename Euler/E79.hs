-- E79.hs

{-# OPTIONS_GHC -Wall #-}

module Euler.E79(e79) where

import Data.List(elemIndex)
import Data.Maybe(isNothing, fromJust)

-- break list of ordered triples (login attempts)
e79 :: Integer
e79 = head $ filter validPassword [1..]
  where
    keylog :: (Integral a) => [a]
    keylog = [319,680,180,690,129,620,762,689,762,318,
              368,710,720,710,629,168,160,689,716,731,
              736,729,316,729,729,710,769,290,719,680,
              318,389,162,289,162,718,729,319,790,680,
              890,362,319,760,316,729,380,319,728,716]

    sLog :: (Integral a, Read a) => [[a]]
    sLog = map splitIntegral keylog

    splitIntegral :: (Integral a, Read a) => a -> [a]
    splitIntegral a = map (\x -> read x) $ map (\x -> [x]) (show a)

    match' :: (Eq a) => [a] -> [a] -> Bool
    match' [] _ = True  -- everthing has been succesfully matched
    match' _ [] = False -- still trying to match more but reached the end of the list
    match' (x:xs) test
      | isNothing (elemIndex x test) = False
      | otherwise                    = match' xs nextTest
      where
        nextTest = drop (idx + 1) test
        idx = fromJust (elemIndex x test)

    validPassword :: Integer -> Bool
    validPassword p = and $ map (\x -> match' x (splitIntegral p)) sLog
