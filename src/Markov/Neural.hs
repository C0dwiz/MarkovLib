-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

module Markov.Neural
  ( Perceptron(..)
  , predict
  , train
  ) where

-- | A simple perceptron model.
data Perceptron = Perceptron
  { weights :: [Double] -- ^ weight vector
  , bias    :: Double   -- ^ bias term
  } deriving (Show, Eq)

-- | Predict the output (0 or 1) for a given input vector.
predict :: Perceptron -> [Double] -> Double
predict (Perceptron w b) xs =
  let s = sum $ zipWith (*) w xs
  in if s + b >= 0 then 1 else 0

-- | Update the perceptron using one training example.
train
  :: Double           -- ^ learning rate
  -> Perceptron       -- ^ initial model
  -> ([Double], Double) -- ^ (input vector, target label)
  -> Perceptron
train eta (Perceptron w b) (xs, d) =
  let y    = predict (Perceptron w b) xs
      err  = d - y
      w'   = zipWith (\wi xi -> wi + eta * err * xi) w xs
      b'   = b + eta * err
  in Perceptron w' b'
