module Config_template
    (
      inDir,stopWordFile,morphLexicon,inDirP, inDirK, outDir,inDirCandidates,outDirCandidates,inDirAnnotated,outDirAnnotated
    ) where
-- GetPhrFreq.hs
inDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/document_set/"
stopWordFile = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "stopwords.txt"
morphLexicon = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "morphLexicon.txt"
-- GP.hs
inDirP = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/candidates/"
inDirK = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/training_set_keyphrases/"
outDir = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/ksm/"
-- GetCandidates.hs
inDirCandidates = ""--HAS TO BE ADDED BEFORE COMPILING AND EXECUTING; example: "/home/user/Documents/training_set/"
outDirCandidates = inDirP
-- GetAnnotated.hs
inDirAnnotated = inDirCandidates
outDirAnnotated = inDirK