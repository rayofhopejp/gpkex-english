module PrepareRunGP where

import GetPhrFreq
import GetCandidates
import GetAnnotated

main :: IO ()
main = do 
    runGetPhrFreq
    runGetCandidates
    runGetAnnotated