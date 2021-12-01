{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module GP
    (
      runGP
    ) where

import Config
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


takeNumber = 300 :: Int--can be changed, 300 documents produces good results in reasonably short time
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
       | Title
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
eval Title c   = Just (cTitle c)

-- todo: this must be changed
{- 
instance GenExpr E where
  -- nodeIndices :: e -> ([Int], [Int])
  nodeIndices (Plus e1 e2) = ([0,1],[0,1])
  -- nodeMapM :: Monad m => (e -> m e) -> e -> m e
  nodeMapM　f (Plus c1 c2) = Plus <$> f c1 <*> f c2
  nodeMapM　f (Minus c1 c2) = Minus <$> f c1 <*> f c2
  nodeMapM　f (Times c1 c2) = Times <$> f c1 <*> f c2
  nodeMapM　f (Div c1 c2) = Div <$> f c1 <*> f c2
  nodeMapM　f (Log c1) = Log <$> f c1
  nodeMapM　f (Inv c1) = Inv <$> f c1
  nodeMapM　f (TimTen c1) = TimTen <$> f c1
  nodeMapM　f (DivTen c1) = DivTen <$> f c1
  nodeMapM　f n = return n
  -- nodes :: e -> Int
  nodes　(Plus c1 c2) = 1 + (nodes c1) + (nodes c2)
  nodes　(Minus c1 c2) = 1 + (nodes c1) + (nodes c2)
  nodes　(Times c1 c2) = 1 + (nodes c1) + (nodes c2)
  nodes　(Div c1 c2) = 1 + (nodes c1) + (nodes c2)
  nodes　(Log c1) = 1 + nodes c1
  nodes　(Inv c1) = 1 + nodes c1
  nodes　(TimTen c1) = 1 + nodes c1
  nodes　(DivTen c1) = 1 + nodes c1
  nodes　_ = 1
  -- depth :: e -> Int
  depth (Plus c1 c2) = 1 + max (depth c1) (depth c2)
  depth (Minus c1 c2) = 1 + max (depth c1) (depth c2)
  depth (Times c1 c2) = 1 + max (depth c1) (depth c2)
  depth (Div c1 c2) = 1 + max (depth c1) (depth c2)
  depth (Log c1) = 1 + depth c1
  depth (Inv c1) = 1 + depth c1
  depth (TimTen c1) = 1 + depth c1
  depth (DivTen c1) = 1 + depth c1
  depth _ = 1
-}

instance GenProg (Rand StdGen) E where
  terminal = do
    r<-getRandomR (0,10)
    return $ [Tf,Idf,Tfidf,First,Last,NumberF,NumberS,NumberT,Length,Rare,Title] !! r
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
  cTitle  :: Float,
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

processFiles fn = do
  s1 <- readFile $ inDirP ++ fn
  s2 <- readFile $ inDirK ++ fn
  let kwrds = read s2 :: [[String]]
  -- リストからstringのリスト(？)をkeyとしCandidateをvalueとするmapを作成
  let cndte = M.fromList $ read s1 :: M.Map [String] Candidate
  return (kwrds,cndte)

runGP :: IO()
runGP = do
  -- get all files in the inDirP
  fns <- getDirectoryContents inDirP 
  -- .txtだけ抽出してソート、takeNumberだけ抽出
  let a = take takeNumber $ filterFiles fns
  -- 全.txtに対してprocessFilesを適用してリストに
  -- 各ファイルを読み込み、[(キーワードのリスト、文章のマップ)]をcandsに束縛
  cands <- forM a (processFiles)
  -- get all files in the outDir
  trs <- getDirectoryContents outDir
  -- outDirの各ファイルを読み込み、
  let nfn = outDir ++ newFileName' trs
  -- fitnessを設定(その他のパラメータはdefault)
  let params = defaultEvolParams { fitness = myFitness cands}
  g <- getStdGen --get a roundom generator with random seeds
  -- evolveTraceは初期集団を作って、終了まで進化を繰り返す。成功した進化状態を返す。
  let trace = evalRand (evolveTrace params {elitists = 1, mProb = 0.05}) g--parameters can be changed here
  let i = cachedBest $ last trace
  writeFile nfn $ show (show $ unInd i, sFitness i)
