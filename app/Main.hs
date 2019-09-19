{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.SBV
import Data.SBV.Control

import Data.SBV.List ((.!!))
import qualified Data.SBV.List as L

count :: Integer -> SList Integer -> SBV Integer -> SBV Integer
count 0 list val = literal 0
count pos list val =
  let (cur, rest) = L.uncons list
      x = (ite (val .== cur) (literal 1) (literal 0))
  in x + (count (pos - 1) rest val)

nestedExample :: Integer -> Symbolic [Integer]
nestedExample s = do
  a :: SList Integer <- sList "a"
  constrain $ L.length a .== (fromIntegral $ s * s)
  let lessThanS i =
        constrain $ (a .!! i .>= 0) .&& (a .!! i) .< (fromIntegral s)
  -- 
  -- All elements must be between 0 and s - 1
  mapM_ (lessThanS . fromIntegral) [(0 :: Int) .. (fromIntegral $ (s * s - 1))]
  --
  -- All elements must appear at max s times
  let countS i = constrain $ (count (s * s) a i) .== (fromIntegral s)
  mapM_ (countS . fromIntegral) [(0 :: Int) .. (fromIntegral $ s - 1)]
  --
  -- Do the query
  query $ do
    cs <- checkSat
    case cs of
      Unk -> error "Solver said unknown!"
      Unsat -> error "Unsat"
      Sat -> getValue a

main :: IO ()
main = do
  s <- runSMT $ nestedExample 4
  print s
