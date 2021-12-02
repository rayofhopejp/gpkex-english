module Config_template
    (
      inDir,stopWordFile,inDirP, inDirK, outDir,inDirCandidates,outDirCandidates,inDirAnnotated,outDirAnnotated,candidateFrequency
      ,inDirExtract,outDirExtract,generatedSamples,trueSamples,evaluateAt
    ) where

-- GetPhrFreq.hs
inDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/lifehacker_train_text/"
stopWordFile = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "stopWordFile.txt"
-- GP.hs
inDirP = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/l_traindata_inDirP/"
inDirK = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/l_traindata_inDirK/"
outDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/l_outdir/"
-- GetCandidates.hs
inDirCandidates = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/lifehacker_train_text/"
outDirCandidates = inDirP
-- GetAnnotated.hs
inDirAnnotated =  ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/lifehacker_train_keywords/"
outDirAnnotated = inDirK
-- kex.hs
candidateFrequency = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "phraseFreq.txt"
inDirExtract = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/lifehacker_test_text/"
outDirExtract = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/l_generatedkeyword/"
-- evaluate.hs
generatedSamples = outDirExtract
trueSamples = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/lifehacker_test_keywords/"
evaluateAt = 5 :: Int -- Precision@evaluateAt, recall@evaluateAt, F1@evaluateAt