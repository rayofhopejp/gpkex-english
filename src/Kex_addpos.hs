{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Kex_addpos
    (
      runKex
    ) where

import Config
import Operations
import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import Data.List.Split
import System.Environment
import System.FilePath
import Data.List
import qualified  NLP.POS  as N
import qualified NLP.Types.Tree as NT
import qualified NLP.Corpora.Conll as NC
import qualified Data.Text as T
import System.IO.Unsafe

--------------------------
--GENPROG
--------------------------
data E = Plus E E
       | Minus E E
       | Times E E
       | Div E E
       | Log E
       | Inv E
       | TimTen E
       | DivTen E
       | Tf
       | Idf
       | Tfidf
       | First
       | Last
       | NumberF
       | NumberS
       | NumberT
       | Length
       | Rare
       | Noun
       | Adv 
       | Verb
       | Adj
       -- todo:change here to add attribute
       deriving (Typeable,Data,Eq,Show,Read)

eval :: E -> Candidate -> Maybe Float
eval (Plus e1 e2) c  = liftM2 (+) (eval e1 c) (eval e2 c)
eval (Minus e1 e2) c = liftM2 (-) (eval e1 c) (eval e2 c)
eval (Times e1 e2) c = liftM2 (*) (eval e1 c) (eval e2 c)
eval (Div e1 e2) c | ok        = liftM2 (/) x1 x2
                   | otherwise = Nothing
  where (x1,x2) = (eval e1 c,eval e2 c)
        ok = x2 /= Just 0
eval (Log e1) c | ok        = liftM  (log) x1
                | otherwise = Nothing
  where x1 = eval e1 c
        ok = x1 > Just 0
eval (Inv e1) c | ok        = liftM  (1.0 / ) x1
                | otherwise = Nothing
  where x1 = eval e1 c
        ok = x1 /= Just 0
eval (TimTen e1) c = liftM (10.0 *) (eval e1 c)
eval (DivTen e1) c = liftM (/ 10.0) (eval e1 c)
eval Tf c      = Just (cTF c)
eval Idf c     = Just (cIDF c)
eval Tfidf c   = Just (cTFIDF c)
eval First c   = Just (cFirst c)
eval Last c    = Just (cLast c)
eval NumberF c = Just (cF c)
eval NumberS c = Just (cS c)
eval NumberT c = Just (cT c)
eval Length c  = Just (cNumber c)
eval Rare c    = Just (cRare c)
eval Noun c    = Just (cNoun c)
eval Adv c    = Just (cAdv c)
eval Verb c    = Just (cVerb c)
eval Adj c    = Just (cAdj c)
-- todo : change when you change E

data Phrase = Phrase {
  pWords :: [String],
  pPos   :: [Int] } deriving (Show, Read, Eq, Ord)

data Candidate = Candidate {
  cTF     :: Float,
  cIDF    :: Float,
  cTFIDF  :: Float,
  cFirst  :: Float,
  cLast   :: Float,
  cF      :: Float,
  cS      :: Float,
  cT      :: Float,
  cNumber :: Float,
  cRare   :: Float,
  cNoun   :: Float,
  cAdv    :: Float,
  cVerb   :: Float,
  cAdj    :: Float,
  -- todo: change when you change Candidates.
  cOrig   :: String } deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------

getDict :: String -> M.Map String [String] -> M.Map String [String]
getDict s m = M.insert fi le m
  where w = words s
        le = filter (\w' -> (isUpper $ head w') && length w' == 1) $ tail w
        fi = map toLower $ head w

getComb :: [String] -> [String] -> M.Map String [String] -> [String]
getComb [] l _ = l
getComb ss [] m = getComb (tail ss) el m
  where el = fin $ M.lookup (head ss) m
        fin (Just x) = x
        fin (Nothing) = ["X"]
getComb ss l m = getComb (tail ss) con m
  where el = fin $ M.lookup (head ss) m
        fin (Just x) = x
        fin (Nothing) = ["X"]
        con = concat $ map (\el' -> map (\l' -> l' ++ el') l) el


takeTuple :: [String] -> [[String]]
takeTuple [] = []
takeTuple w
 | l >= 4    = [take 4 w] ++ [take 3 w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l == 3    = [w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l >= 2    = [w] ++ [[head w]] ++ [tail w]
 | otherwise = [w]
  where l = length w

splitText :: String -> String -> [String]
splitText word []     = [word]
splitText word (t:ts)
  | not (isAlpha t || isSpace t || isDigit t || elem t "-'\"") = [word] ++ splitText "" ts
  | isDigit t || elem t "-'\"" = splitText word ts
  | isSpace t = splitText (word ++ " ") ts
  | otherwise = splitText (word ++ [toLower t]) ts

makeTuples :: String -> [[String]]
makeTuples = concat . map takeTuple . map words . splitText "" . filter (\x -> not (elem x "€—£§«»<@♦¬°►•[_{„¥©>^~■®▼]"))

stem2 :: String -> String
stem2 "" = error "stemming empty string"
stem2 s
  | i >= stemN && i > div (length s) 2 = take i s
  | otherwise                      = s
  where i = if fi == [] then 10 else maximum fi
        fi = findIndices (\a -> elem a "aeiouAEIOU") s

stem :: [String] -> [String]
stem (x:[]) = [stem2 x]
stem xs     = map (take 5) xs

removeStop :: S.Set String -> [String] -> Bool
removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)

getTextOnly :: String -> String
getTextOnly s = head $ splitOn "</body>" $ last $ splitOn "<body>" s

makeTuples' :: String -> [String]
makeTuples' = stem . words

getKeywords :: String ->  [[[String]]]
getKeywords s = map getKW . tail $ splitOn "<keywords " s
  where getKW st = map (makeTuples' . map toLower . head . splitOn "</keyword>") . tail $ splitOn "<keyword>" st

getKeys :: String -> ([[String]], [[String]])
getKeys s = (uniq, allk)
  where kw = getKeywords s
        kw' = concat kw
        uniq = foldl (\l k -> if elem k l then l else k : l) [] kw'
        allk = foldl (\a b -> filter (\a' -> elem a' b) a) (head kw) (tail kw)

getPhrases :: [[String]] -> M.Map [String] Phrase
getPhrases s = foldl (\map (i,c) -> M.insertWith addPhrase (stem c) (Phrase c [i]) map) M.empty cand
  where cand = zip [1..] s
        addPhrase (Phrase c1 is) (Phrase c2 is') = Phrase c1 (is' ++ is)

onlyOneword :: M.Map [String] Phrase -> M.Map [String] Int -> Int -> S.Set (Float, String)
onlyOneword m dfm l = M.foldrWithKey (\k o s -> S.insert (tfidf k o,w o) s) S.empty okW
  where onlyOne (x:[]) = True
        onlyOne _      = False
        okW            = M.filter (\(Phrase w _) -> onlyOne w) m
        w (Phrase (x:[]) _ )  = x
        p (Phrase _      l')  = fromIntegral $ length l'
        tfidf k o             = ((p o) / (fromIntegral l)) * log ((fromIntegral $ takeNumber) / (fromIntegral $ getdfm k $ M.lookup k dfm))
        getdfm k (Nothing)    = 1
        getdfm k (Just a)     = a


takeBest :: S.Set (Float, String) -> S.Set String
takeBest s = S.fromList $ map (\(f,s') -> s') $ take rareNumber $ reverse $ S.toList s

rankCandidates :: (Candidate -> Maybe Float) -> M.Map [String] Candidate -> Int -> [String]
rankCandidates evF m i = take i $ map snd l
  where l = sort $ M.foldrWithKey (\k c s -> (solve (evF c), cOrig c) : s) [] m
        solve (Nothing) = 1000000
        solve (Just a ) = a

-- todo: get noun num
nounlist = [NC.NN, NC.NNP, NC.NNPS, NC.NNS]
adverblist = [NC.RB, NC.RBR, NC.RBS]
verblist = [NC.VB, NC.VBD, NC.VBG, NC.VBN, NC.VBP, NC.VBZ]
adjlist = [NC.JJ, NC.JJR, NC.JJS]
taggedSenttoPoslist :: NT.TaggedSentence NC.Tag->  [NT.POS NC.Tag]
taggedSenttoPoslist (NT.TaggedSent xs) =  xs
taggedSentlisttoPoslist ::  [NT.TaggedSentence NC.Tag] ->  [NT.POS NC.Tag]
taggedSentlisttoPoslist [] = []
taggedSentlisttoPoslist xs  =  concat $ map taggedSenttoPoslist xs
getnum :: [NC.Tag] -> [String] -> Int
getnum lst s = length $ filter (\x -> x `elem` lst)$ map NT.posTag  $ taggedSentlisttoPoslist $ N.tag tagger $ T.pack $ intercalate " " s
  where tagger = unsafePerformIO　N.defaultTagger
-- todo:change here while you change candidate
makeCandidates :: M.Map [String] Phrase -> S.Set String -> M.Map [String] Int -> Int -> M.Map [String] Candidate
makeCandidates phrases toponeword dfm doclen = M.mapWithKey (\k a -> mkC k a) phrases
  where tf (Phrase _ l)       = (fromIntegral $ length l) / (fromIntegral doclen)
        idf k                 = (fromIntegral $ takeNumber) / (fromIntegral $ getdfm k $ M.lookup k dfm)
        getdfm k (Nothing)    = 1
        getdfm k (Just a)     = a
        cfirst (Phrase _ l)   = fromIntegral $ head l
        clast (Phrase _ l)    = fromIntegral $ last l
        oneTh                 = div doclen 3
        first (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' <= oneTh) l
        secon (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' > oneTh && l' <= (2*oneTh)) l
        third (Phrase _ l)    = fromIntegral $ length $ filter (\l' -> l' > (2*oneTh)) l
        numb k                = fromIntegral $ length k
        rare k                | length k == 1 = fromIntegral $ length $ filter (\k' -> S.member k' toponeword) k
                              | otherwise     = fromIntegral $ length $ filter (\k' -> S.member k' toponeword') k
        noun k                = fromIntegral $ getnum nounlist k
        adverb k              = fromIntegral $ getnum adverblist k
        verb k                = fromIntegral $ getnum verblist k
        adj k                 = fromIntegral $ getnum adjlist k
        toponeword'           = S.map (\str -> take 5 str) toponeword
        orig (Phrase w _)     = unwords w
        mkC k a = Candidate (tf a) (idf k) ((tf a) * log(idf k)) (cfirst a) (clast a) (first a) (secon a) (third a) (numb k) (rare k) (noun k) (adverb k) (verb k) (adj k) (orig a)


---------------------------------------------------------------------

stemN = 5 :: Int
rareNumber = 10 :: Int
takeNumber = 300 :: Int
takeSample = 60 :: Int

-- main
runKex = do
  args <- getArgs --コマンドライン引数を取得
  let tr = head $ args --コマンドライン引数の先頭の文字列は木構造のファイル名
  fns' <- getDirectoryContents inDirExtract
  let fns = filterFiles fns'
  sws <- readFile stopWordFile
  s' <- readFile candidateFrequency
  tree' <- readFile tr
  let stopW = S.fromList $ lines sws
  let tree'' = read $ tree' :: (String,Float)
  let tree = read $ fst $ tree'' :: E
  let pfq = M.fromList $ read s' -- phrase frequency
  forM fns (processFiles stopW tree pfq)
  return ()

processFiles stopW tree pfq fn = do
  txt <- readFile fn
  let phr = getPhrases $ filter (\ph -> filterPOS ph) $ filter (removeStop stopW) $ makeTuples txt
  let len = M.size phr
  let owp = onlyOneword phr pfq len
  let can = makeCandidates phr (takeBest owp) pfq len
  let mRes = rankCandidates (eval tree) can 10
  let nfn = outDirExtract ++ (snd $ splitFileName fn)
  writeFile nfn $ unlines mRes

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDirExtract++ f) $ sort $ filter (\f -> isSuffixOf ".txt" f) fs
  
