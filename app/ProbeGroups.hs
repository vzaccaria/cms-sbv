{-# LANGUAGE ScopedTypeVariables #-}

module ProbeGroups where

import           Data.List

countPartitions :: (Ord a, Num a) => a -> [a] -> Int
countPartitions nshares list =
  let la = sort list
      lb = tail la ++ [(head la + nshares)]
  in length $ filter (> 1) $ zipWith (-) lb la

getBoundD :: (Ord a, Num a) => a -> [a] -> Int
getBoundD nshares list =
  let pi = length list
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
