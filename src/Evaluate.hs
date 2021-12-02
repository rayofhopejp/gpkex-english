module Evaluate
    (
      runEvaluate
    ) where

import Config
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
import Control.Applicative

runEvaluate :: IO ()
runEvaluate = do
  fns <- getDirectoryContents generatedSamples
  let gfns = filterFiles fns generatedSamples
  let tfns = filterFiles fns trueSamples
  let zipfns = getZipList $ (,) <$> ZipList gfns <*> ZipList tfns
  -- gfnsとtfnsから一個ずつ比較していく
  results <- forM zipfns processFiles
  let precision = average [(\(p, _, _) -> p) x | x <- results] 
  let recall = average  [(\(_, r, _) -> r) x | x <- results] 
  let f1 = average [(\(_, _, q) -> q) x | x <- results] 
  print (precision,recall,f1)
  return ()

processFiles :: (FilePath, FilePath) -> IO (Float,Float,Float)
processFiles (gfn,tfn) = do
  gs' <- readFile gfn
  let gs = take evaluateAt $ lines gs'
  ts' <- readFile tfn
  let ts = lines ts'
  let hit = genericLength $ filter (\x -> x `elem` ts) gs
  let precision = if length gs > 0 then hit / genericLength gs else 0
  let recall = if length ts > 0 then hit / genericLength ts else 0
  let f1 = if precision + recall > 0 then 2 * precision * recall / (precision + recall) else 0
  return (precision,recall,f1)

filterFiles :: [FilePath] -> FilePath -> [FilePath]
filterFiles [] _ = error "empty folder"
filterFiles fs dir = map (\f -> dir++ f) $ sort $ filter (\f -> isSuffixOf ".txt" f) fs

average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)
