-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

module Main where

import Markov.Chain
import Markov.Hidden
import Markov.Neural
import System.Random (getStdGen)

main :: IO ()
main = do
  putStrLn "\n=== Markov Chain Example ==="
  gen <- getStdGen
  let statesMC = ["Sunny","Rainy"]
      transMC  = [[0.8,0.2],[0.5,0.5]]
      chainMC  = fromMatrix statesMC transMC
      pathMC   = simulate 5 chainMC "Sunny" gen
  print pathMC

  putStrLn "\n=== Hidden Markov Model Example ==="
  let hmm = HMM
        ["Hot","Cold"]
        ["1","2"]
        [0.8,0.2]
        [[0.6,0.4],[0.5,0.5]]
        [[0.7,0.3],[0.4,0.6]]
      obs = ["1","2","1"]
  let probF = forward hmm obs
  putStrLn $ "Forward probability: " ++ show probF

  let (bestPath, bestProb) = viterbi hmm obs
  putStrLn $ "Best hidden sequence: " ++ show bestPath
  putStrLn $ "Viterbi probability: " ++ show bestProb

  putStrLn "\n=== Perceptron Example ==="
  let p0 = Perceptron [0,0] 0
      trainingData = [ ([0,0],0), ([1,0],1), ([0,1],1), ([1,1],1) ]
      trained = foldl (train 0.2) p0 trainingData
  print trained
  print $ map (predict trained . fst) trainingData
