-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

module Utils
  ( normalize
  ) where

-- | Normalize a probability vector so that it sums to 1.
normalize :: [Double] -- ^ raw weights
          -> [Double] -- ^ normalized probabilities
normalize xs =
  let s = sum xs
  in if s == 0 then xs else map (/ s) xs
