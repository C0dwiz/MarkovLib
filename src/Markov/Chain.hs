-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

module Markov.Chain
  ( Chain
  , fromMatrix
  , step
  , simulate
  ) where

import System.Random (RandomGen, randomR)

-- | A Markov chain with states of type @s@ and transition probabilities.
data Chain s = Chain
  { states :: [s]
  , matrix :: [[Double]]
  } deriving (Show, Eq)

-- | Create a Markov chain from a list of states and a transition matrix.
fromMatrix :: [s] -> [[Double]] -> Chain s
fromMatrix ss m = Chain ss m

-- | Perform one transition for the given state.
step :: (Eq s, RandomGen g) => Chain s -> s -> g -> (s, g)
step (Chain ss m) current gen =
  case lookup current (zip ss [0..]) of
    Nothing  -> error "step: unknown state"
    Just idx ->
      let row  = m !! idx
          (r, g') = randomR (0.0, 1.0) gen
          pickIdx _ [] = error "step: bad matrix"
          pickIdx p (x:xs) = if p <= x then 0 else 1 + pickIdx (p - x) xs
          i = pickIdx r row
      in (ss !! i, g')

-- | Simulate the Markov chain for a number of steps.
simulate
  :: (Eq s, RandomGen g)
  => Int        -- ^ number of steps
  -> Chain s    -- ^ Markov chain
  -> s          -- ^ initial state
  -> g          -- ^ random generator
  -> [s]
simulate 0 _ start _ = [start]
simulate n c start gen =
  let (next, gen') = step c start gen
  in start : simulate (n-1) c next gen'
