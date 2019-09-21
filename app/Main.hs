{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import qualified Data.Matrix as M
import Data.SBV
import Data.SBV.Control

import Data.SBV.List ((.!!))
import qualified Data.SBV.List as L

count :: Integer -> SList Integer -> Integer -> SBV Integer
count 0 list val = literal 0
count pos list val =
  let (cur, rest) = L.uncons list
      x = (ite (literal val .== cur) (literal 1) (literal 0))
  in x + (count (pos - 1) rest val)

nestedExample :: Integer -> Symbolic [Integer]
nestedExample s = do
  a :: SList Integer <- sList "a"
  let lessThanS i =
        constrain $ (a .!! i .>= 0) .&& (a .!! i) .< (fromIntegral s)
  --
  --
  -- Count indexes of rows
  let p2ij p = ((div p s), (mod p s))
      ij2p (i, j) = (i * s + j)
  let countIndex :: Bool -> Integer -> Integer -> [Integer] -> SBV Word64
      countIndex checkRow _ _ [] = (literal 0 :: SBV Word64)
      countIndex checkRow 0 tpos l@(_:vs) = countIndex checkRow tpos tpos vs
      countIndex checkRow pos tpos l@(val:vs) =
        let (i, j) = p2ij (pos - 1)
            rest = (countIndex checkRow (pos - 1) tpos l)
            xl =
              ite
                (literal val .== a .!! (fromInteger (pos - 1)))
                (setBit
                   rest
                   (fromIntegral
                      (if checkRow
                         then i
                         else j)))
                rest
        in xl
      maxBShares sh = sPopCount $ countIndex False (s * s) (s * s) sh
      maxAShares sh = sPopCount $ countIndex True (s * s) (s * s) sh
  --
  -- Create the transpose
  --
  --
  -- Constrain length of list
  constrain $ L.length a .== (fromIntegral $ s * s)
  --
  -- All elements must be between 0 and s - 1
  mapM_ (lessThanS . fromIntegral) [0 .. (s * s - 1)]
  --
  -- All shares must appear at max s times
  let countS i = constrain $ (count (s * s) a i) .== (fromIntegral s)
  mapM_ (countS) [0 .. s - 1]
  --
  -- Shares must appear in each row and column at max s-1 times
  -- mapM_ (ensureShareLessThan s a) [0 .. s - 1]
  -- Do the query
  let constSh v x = do
        constrain $ maxBShares x .< (fromIntegral v)
        constrain $ maxAShares x .< (fromIntegral v)
  constSh 4 [0]
  constSh 4 [1]
  constSh 4 [2]
  constSh 4 [3]
  -- constSh 5 [1, 0]
  -- constSh 5 [3, 0]
  -- constSh 5 [2, 1]
  -- constSh 5 [3, 2]
  query $ do
    cs <- checkSat
    case cs of
      Unk -> error "Solver said unknown!"
      Unsat -> error "Unsat"
      Sat -> do
        av <- getValue a
        return av

asMatrix shares list = M.fromList shares shares list

main :: IO ()
main = do
  let shares = 4 :: Int
  s <- runSMT $ nestedExample (fromIntegral shares)
  print $ asMatrix shares s
