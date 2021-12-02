module GetCandidates
    (
      runGetCandidates
    ) where

import Config
import Operations
import Data.Char
import Data.List
import System.IO
import Data.List.Split
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory
import System.FilePath


stemN = 5 :: Int
rareNumber = 10 :: Int
takeNumber = 960 :: Int

data Phrase = Phrase {
  pWords :: [String],
  pPos   :: [Int] } deriving (Show, Read, Eq, Ord)

data Candidate = Candidate {
  cTF     :: Float,
  cIDF    :: Float,
  cTFIDF  :: Float,
  cFirst  :: Int,
  cLast   :: Int,
  cF      :: Int,
  cS      :: Int,
  cT      :: Int,
  cNumber :: Int,
  cRare   :: Int,
  cTitle  :: Int,
  cOrig   :: String } deriving (Show, Read, Eq, Ord)

getDict :: String -> M.Map String [String] -> M.Map String [String]
getDict s m = M.insert fi le m
  where w = words s
        le = filter (\w' -> (isUpper $ head w') && length w' == 1) $ tail w
        fi = map toLower $ head w

getLemma :: String -> M.Map String String -> M.Map String String
getLemma s m = M.insert fi le m
  where w = words s
        fi = map toLower $ head w
        le = map (\c -> toLower c) $ head $ tail $ w



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
  

splitText :: String -> String -> [String]
splitText word []     = [word]
splitText word (t:ts)
  | not (isAlpha t || isSpace t || isDigit t || elem t "-'\"") = [word] ++ splitText "" ts
  | isDigit t || elem t "-'\"" = splitText word ts
  | isSpace t = splitText (word ++ " ") ts
  | otherwise = splitText (word ++ [toLower t]) ts


makeTuples :: String -> [[String]]
makeTuples = concat . map takeTuple . map words . splitText "" . filter (\x -> not (elem x "€—£§«»<@♦¬°►•[_{„¥©>^~■®▼]"))


takeTuple :: [String] -> [[String]]
takeTuple [] = []
takeTuple w
 | l >= 4    = [take 4 w] ++ [take 3 w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l == 3    = [w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l >= 2    = [w] ++ [[head w]] ++ [tail w]
 | otherwise = [w]
  where l = length w


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

getTextOnly :: String -> String
getTextOnly s = head $ splitOn "</body>" $ last $ splitOn "<body>" s

getTitle :: String -> String
getTitle s = head $ splitOn "</title>" $ last $ splitOn "<title>" s


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
        getdfm k (Nothing)    = error $ "only" ++ show k
        getdfm k (Just a)     = a

        
takeBest :: S.Set (Float, String) -> S.Set String
takeBest s = S.fromList $ map (\(f,s') -> s') $ take rareNumber $ reverse $ S.toList s

makeCandidates :: M.Map [String] Phrase -> S.Set String -> M.Map [String] Int -> Int -> M.Map [String] Candidate
makeCandidates phrases toponeword dfm doclen = M.mapWithKey (\k a -> mkC k a) phrases
  where tf (Phrase _ l)       = (fromIntegral $ length l) / (fromIntegral doclen)
        idf k                 = (fromIntegral $ takeNumber) / (fromIntegral $ getdfm k $ M.lookup k dfm)
        getdfm k (Nothing)    = error $ show k
        getdfm k (Just a)     = a
        cfirst (Phrase _ l)   = head l
        clast (Phrase _ l)    = last l
        oneTh                 = div doclen 3
        first (Phrase _ l)    = length $ filter (\l' -> l' <= oneTh) l
        secon (Phrase _ l)    = length $ filter (\l' -> l' > oneTh && l' <= (2*oneTh)) l
        third (Phrase _ l)    = length $ filter (\l' -> l' > (2*oneTh)) l
        numb k                = length k
        rare k                | length k == 1 = length $ filter (\k' -> S.member k' toponeword) k
                              | otherwise     = length $ filter (\k' -> S.member k' toponeword') k
        toponeword'           = S.map (\str -> take 5 str) toponeword
        titl k                = 0 -- todo: delete title from Candidates
        orig (Phrase w _)     = unwords w
        mkC k a = Candidate (tf a) (idf k) ((tf a) * log(idf k)) (cfirst a) (clast a) (first a) (secon a) (third a) (numb k) (rare k) (titl k) (orig a)

-- main
runGetCandidates = do
  sws <- readFile stopWordFile
  let stopW = S.fromList $ lines sws
  fns <- getDirectoryContents inDirCandidates
  s' <- readFile "phraseFreq.txt"
  let pfq = M.fromList $ read s' -- key:["apple","watch"] value:1
  let a = take takeNumber $ filterFiles fns
  forM a (processFiles stopW pfq)
  return ()

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDirCandidates ++ f) $ sort $ filter (\f -> isSuffixOf ".txt" f) fs

processFiles stopW pfq fn = do
  s <- readFile fn
  let phr = getPhrases $ map (\ph -> lemmatize ph) $ filter (\ph -> filterPOS ph) $ filter (removeStop stopW) $ makeTuples s
  let len = M.size phr
  let owp = onlyOneword phr pfq len
  let can = makeCandidates phr (takeBest owp) pfq len
  let nfn = outDirCandidates ++ (snd $ splitFileName fn)
  writeFile nfn $ show $  M.toList can

removeStop :: S.Set String -> [String] -> Bool
removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)
