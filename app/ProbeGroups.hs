{-# LANGUAGE ScopedTypeVariables #-}

module ProbeGroups where

import           Data.List

countPartitions :: Integer -> [Integer] -> Int
countPartitions nshares list =
  let la = sort list
      lb = tail la ++ [(head la + nshares)]
  in fromIntegral $ length $ filter (> 1) $ zipWith (-) lb la

getBoundD :: Integer -> [Integer] -> (Integer, Int)
getBoundD nshares list =
  let pi = fromIntegral $ length list
      t = countPartitions nshares list
  in (pi + 1 + 2 * (fromIntegral t), t)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = xss /\/ map (x :) xss
  where
    xss = powerset xs

ps x = tail $ powerset x

(/\/) :: [a] -> [a] -> [a]
[] /\/ ys = ys
(x:xs) /\/ ys = x : (ys /\/ xs)

getBoundProbes :: Integer -> [([Integer], (Integer, Int))]
getBoundProbes nshares =
  let addD l = (l, getBoundD nshares l)
      isWorthChecking (l, (d, _)) = d <= (nshares)
  in filter isWorthChecking $ map addD (ps [0 .. nshares - 1])

getBoundProbesAll :: Integer -> [([Integer], (Integer, Int))]
getBoundProbesAll nshares =
  let addD l = (l, getBoundD nshares l)
  in map addD (ps [0 .. nshares - 1])
