module Main where

import Lib
import GP
import GetPhrFreq
import GetCandidates
import GetAnnotated

main :: IO ()
-- main = runGP
-- main = runGetPhrFreq
main = runGetCandidates
-- main = runGetAnnotated
-- main = someFunc