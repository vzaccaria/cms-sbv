#!/usr/bin/env stack
{-
  stack --resolver lts-14.6 script
  --package sbv 
-}
{-# LANGUAGE ScopedTypeVariables #-}

module RSNI where

-- import           ProbeGroups
import Data.List
import Data.SBV
import Data.SBV.Control

import Data.SBV.List ((.!!))
import qualified Data.SBV.List as L
import ProbeGroups

possibleProbes s =
  filter (\l -> length l < s && length l > 0) $ powerset [0 .. s - 1]

allocateRandom :: Integer -> Integer -> Symbolic [Word64]
allocateRandom n s = do
  a <- mkFreeVars (fromInteger $ s * s)
  let batchXor xs = foldl (\acc v -> acc `xor` (a !! v)) (literal 0) xs
  let fixRandomPair i = constrain $ sPopCount (a !! i) .== 2
  let limitRandom i = constrain $ (a !! i) .< (2 ^ (s * s))
  mapM_ fixRandomPair [0 .. (fromInteger n) - 1]
  mapM_ limitRandom [0 .. (fromInteger n) - 1]
  query $ do
    cs <- checkSat
    case cs of
      Unk -> error "Solver said unknown!"
      Unsat -> error "Unsat"
      Sat -> do
        av <- sequence $ map getValue a
        return av

main :: IO ()
main = do
  let shares = 3 :: Integer
      randoms = 2 :: Integer
  s <- runSMT $ allocateRandom randoms shares
  print $ s
