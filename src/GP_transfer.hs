{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module GP_transfer
    (
      runGP
    ) where

import Config
import Operations
import GenProg
import GenProg.GenExpr
import GenProg.GenExpr.Data
import Data.Generics
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Maybe
import System.Directory
import Text.Read
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

takeNumber = 100000 :: Int--can be changed, 300 documents produces good results in reasonably short time
infi = 1 / 0

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
       -- todo: change here to add new attribute 
       deriving (Typeable,Data,Eq,Show)

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
eval Adv c     = Just (cAdv c)
eval Verb c    = Just (cVerb c)
eval Adj c     = Just (cAdj c)
-- todo: change when you change E.

instance GenProg (Rand StdGen) E where
  terminal = do
    r<-getRandomR (0,13) -- todo: change when you change E. [low,hi].
    return $ [Tf,Idf,Tfidf,First,Last,NumberF,NumberS,NumberT,Length,Rare,Noun,Adv,Verb,Adj] !! r
  nonterminal = do
    r <- getRandomR (0,7)
    [liftM2 Plus terminal terminal,
     liftM2 Minus terminal terminal,
     liftM2 Times terminal terminal,
     liftM2 Div terminal terminal,
     liftM TimTen terminal,
     liftM DivTen terminal,
     liftM Log terminal,
     liftM Inv terminal] !! r

myFitness :: [([[String]], M.Map [String] Candidate)] -> E -> Double
myFitness xs e = error-- * size--  size is used only for introducing parsimony pressure
  where error = realToFrac $ (1000 / ) $ sum $ map (\(kphrs, m) -> newScore kphrs $ rankCandidates (eval e) m) xs
        size  = ((realToFrac ( nodes e) / 100) + 1.0 )


--------------------------

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
  -- todo: change here when you change candidate
  cOrig   :: String } deriving (Show, Read, Eq, Ord)


rankCandidates :: (Candidate -> Maybe Float) -> M.Map [String] Candidate -> M.Map [String] Int
rankCandidates evF m = foldl (\m' (i,(_,k)) -> M.insert k i m') M.empty l
  where l = zip [1..] $ sort $ M.foldrWithKey (\k c s -> (solve (evF c), k) : s) [] m
        solve (Nothing) = infi
        solve (Just a ) = a

newScore :: [[String]] -> M.Map [String] Int -> Float
newScore q res = if le /= 0 then good else 1 / minBad
  where correct = M.foldrWithKey (\k v l -> if v <= num && elem k q then (v,k) : l else l) [] res
        num     = 15
        fi      = fromIntegral $ fst $ minimum correct
        le      = fromIntegral $ length correct
        good    = le / fi
        minBad  = fromIntegral $ M.foldrWithKey (\k v l -> if v < l && elem k q then v else l) 1000000000 res


desc :: (Ord a) => a -> a -> Ordering
desc a b | a < b     = Prelude.GT
         | a > b     = LT
         | otherwise = EQ

rankCandidates' :: (Candidate -> Maybe Float) -> M.Map [String] Candidate -> [String]
rankCandidates' evF m = map (\(n, k) -> cOrig $ fromJust $ M.lookup k m) $ sort $ M.foldrWithKey (\k c s -> (solve (evF c), k) : s) [] m
  where solve (Nothing) = 0
        solve (Just a ) = a

filterFiles [] = error "empty folder"
filterFiles fs = sort $ filter (\f -> isSuffixOf ".txt" f) fs

newFileName :: [String] -> String
newFileName [] = "0001.txt"
newFileName fs = replicate (4 - length mnr) '0' ++ mnr ++ ".txt"
  where nrs = map (\f -> read $ take 4 f) fs :: [Int]
        mnr = show $ 1 + maximum nrs

newFileName' :: [String] -> String
newFileName' fs = newFileName $ filter (\f -> isSuffixOf ".txt" f) fs


readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile

processFiles fn = do
  s1 <- readFileStrict ( inDirP ++ fn )
  s2 <- readFileStrict ( inDirK ++ fn )
  let maybekwrds = readMaybe s2 :: Maybe [[String]]
  let maybecndte = readMaybe s1 :: Maybe [([String],Candidate)]
  case maybekwrds of
    Nothing -> error "error in keywords"
    Just kwrds -> case maybecndte of
      Nothing -> error "error in candidates"
      Just cndte -> do
        return (kwrds,M.fromList cndte)
processFiles2 fn = do
  s1 <- readFileStrict ( inDirP2 ++ fn )
  s2 <- readFileStrict ( inDirK2 ++ fn )
  let maybekwrds = readMaybe s2 :: Maybe [[String]]
  let maybecndte = readMaybe s1 :: Maybe [([String],Candidate)]
  case maybekwrds of
    Nothing -> error "error in keywords"
    Just kwrds -> case maybecndte of
      Nothing -> error "error in candidates"
      Just cndte -> do
        return (kwrds,M.fromList cndte)
  

runGP :: IO()
runGP = do
  -- get all files in the inDirP
  fns <- getDirectoryContents inDirP 
  fns2 <- getDirectoryContents inDirP2
  -- .txtだけ抽出してソート、takeNumberだけ抽出
  let a = take takeNumber $ filterFiles fns
  let b = take takeNumber $ filterFiles fns2
  -- 全.txtに対してprocessFilesを適用してリストに
  -- 各ファイルを読み込み、[(キーワードのリスト、文章のマップ)]をcandsに束縛
  cands <- forM a processFiles
  cands2 <- forM b processFiles2
  -- get all files in the outDir
  trs <- getDirectoryContents outDir
  let nfn = outDir ++ newFileName' trs
  -- fitnessを設定(その他のパラメータはdefault)
  let params = defaultEvolParams { fitness = myFitness cands}
  let params2 = defaultEvolParams { fitness = myFitness cands2}
  g <- getStdGen --get a roundom generator with random seeds
  -- inDirP(K)2のファイルを使って先に学習
  let population = pop $ last $ evalRand ( evolveTrace params2 {terminate=tGeneration 50, elitists = 1, mProb = 0.05}) g
  -- traceからさらに学習
  let trace = evalRand (evolveTraceFrom params {terminate=tGeneration 50, elitists = 1, mProb = 0.05} population) g
  let i = cachedBest $ last trace
  writeFile nfn $ show (show $ unInd i, sFitness i)
