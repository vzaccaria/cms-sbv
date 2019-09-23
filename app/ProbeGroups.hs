{-# LANGUAGE ScopedTypeVariables #-}

module ProbeGroups where

import Data.List

countPartitions :: Integer -> [Integer] -> Integer
countPartitions nshares list =
  let la = sort list
      lb = tail la ++ [(head la + nshares)]
  in fromIntegral $ length $ filter (> 1) $ zipWith (-) lb la

getBoundD :: Integer -> [Integer] -> Integer
getBoundD nshares list =
  let pi = fromIntegral $ length list
      t = countPartitions nshares list
  in pi + 1 + 2 * t

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss /\/ map (x :) xss
  where
    xss = powerset xs

ps x = tail $ powerset x

(/\/) :: [a] -> [a] -> [a]
[] /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

getBoundProbes nshares =
  let addD l = (l, getBoundD nshares l)
      isWorthChecking (l, d) = d <= (nshares)
  in filter isWorthChecking $ map addD (ps [0 .. nshares - 1])
