module PrepareRunGP where

import GetPhrFreq
import GetCandidates_addpos
import GetAnnotated

main :: IO ()
main = do 
    runGetPhrFreq
    runGetCandidates
    runGetAnnotated