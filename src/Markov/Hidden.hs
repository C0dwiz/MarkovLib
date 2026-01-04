-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

{-# LANGUAGE TupleSections #-}

module Markov.Hidden
  ( HMM(..)
  , forward
  , viterbi
  ) where

import Data.List (maximumBy, foldl')
import Data.Ord (comparing)

-- | A Hidden Markov Model with hidden states of type @s@ and observations of type @o@.
data HMM s o = HMM
  { hStates   :: [s]        -- ^ Hidden state labels
  , oSymbols  :: [o]        -- ^ Observable symbol labels
  , startProb :: [Double]   -- ^ Initial state probability vector
  , transProb :: [[Double]] -- ^ Transition probability matrix
  , emitProb  :: [[Double]] -- ^ Emission probability matrix
  } deriving (Show, Eq)

-- | Computes the probability of a sequence of observations using the Forward algorithm.
forward :: Eq o => HMM s o -> [o] -> Double
forward _ [] = 0
forward (HMM ss os pi a b) obs =
  let indexOf x = case lookup x (zip os [0..]) of
                    Just i  -> i
                    Nothing -> error "forward: unknown observation"
      firstO = indexOf (head obs)
      alpha0 = zipWith (*) pi [ b !! j !! firstO | j <- [0..length ss - 1] ]
      stepF alpha o =
        let oi = indexOf o
        in [ sum [ alpha !! j * (a !! j !! i) | j <- [0..length ss - 1] ]
             * (b !! i !! oi)
           | i <- [0..length ss - 1] ]
  in sum $ foldl' stepF alpha0 (tail obs)

-- | Performs the Viterbi algorithm.
viterbi
  :: Eq o
  => HMM s o       -- ^ Hidden Markov Model
  -> [o]           -- ^ Observation sequence
  -> ([s], Double) -- ^ (best hidden sequence, its probability)
viterbi _ [] = ([], 0)
viterbi (HMM ss os pi a b) obs =
  let
    indexOf x = case lookup x (zip os [0..]) of
                  Just i  -> i
                  Nothing -> error "viterbi: unknown observation"
    nStates = length ss
    firstO = indexOf (head obs)

    delta0 = [ pi !! i * b !! i !! firstO | i <- [0..nStates - 1] ]
    psi0   = replicate nStates 0

    stepV (prevDelta, _, psiAcc) currO =
      let oi = indexOf currO
          (nextDel, nextPsi) =
            unzip
            [ let candidates = [ (prevDelta !! j * a !! j !! i, j) | j <- [0..nStates - 1] ]
                  (bestP, bestIdx) = maximumBy (comparing fst) candidates
              in (bestP * b !! i !! oi, bestIdx)
            | i <- [0..nStates - 1] ]
      in (nextDel, nextPsi, psiAcc ++ [nextPsi])

    (finalDelta, _, psiTable) = foldl' stepV (delta0, psi0, []) (tail obs)

    (bestProb, bestIdx) =
      maximumBy (comparing fst) (zip finalDelta [0..])

    buildPath psis stateIdx t
      | t < 0 = [stateIdx]
      | otherwise =
          let prevState = (psis !! t) !! stateIdx
          in prevState : buildPath psis prevState (t - 1)

    statesIdxs =
      let reversed = bestIdx : buildPath psiTable bestIdx (length psiTable - 1)
      in reverse reversed

  in (map (ss !!) statesIdxs, bestProb)
