-- SPDX-License-Identifier: MIT
-- Copyright (C) 2026 CodWiz

import Test.HUnit
import Markov.Chain
import System.Random (mkStdGen)

testChain = TestCase $
  let ch = fromMatrix [0,1] [[0.5,0.5],[0.5,0.5]]
      path = simulate 2 ch 0 (mkStdGen 42)
  in assertEqual "length of sequence" 3 (length path)

main :: IO ()
main = runTestTT testChain >> return ()
