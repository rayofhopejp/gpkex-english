module GetAnnotated
    (
      runGetAnnotated
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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readFileStrict :: FilePath -> IO String
readFileStrict = fmap T.unpack . TIO.readFile

takeNumber = 100000 :: Int -- default:1000

splitText :: String -> String -> String -> [String]
splitText chars word []     = [word]
splitText []    _    _      = error "invalid stop char sequence"
splitText chars word (t:ts)
  | elem t chars = [word] ++ splitText chars "" ts
  | not (isAlpha t || isSpace t) = splitText chars word ts
  | otherwise    = splitText chars (word ++ [toLower t]) ts

makeTuples :: S.Set String -> String -> [[String]]
makeTuples stopW = filter (removeStop stopW) . concat . map (filter (\ss -> ss /= [] && length ss <= 4) . map (stem . words) . splitText "!\"'()+.,;:?\n"
  "") . splitText "\n" ""
stem2 :: String -> String
stem2 "" = error "stemming empty string"
stem2 s
  | i >= 5 && i > div (length s) 2 = take i s
  | otherwise                      = s
  where i = if fi == [] then 10 else maximum fi
        fi = findIndices (\a -> elem a "aeiouAEIOU") s

stem :: [String] -> [String]
stem (x:[]) = [stem2 x]
stem xs     = map (take 5) xs

-- main
runGetAnnotated = do
  sws <- readFile stopWordFile
  let stopW = S.fromList $ lines sws
  fns <- getDirectoryContents inDirAnnotated
  let a = take takeNumber $ filterFiles fns
  forM a (processFiles stopW)
  return ()

processFiles stopW fn = do
  s <- readFileStrict fn
  let kws = makeTuples stopW $ s
  let nfn = outDirAnnotated ++ (snd $ splitFileName fn)
  writeFile nfn $ show kws

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDirAnnotated ++ f) $ sort $ filter (\f -> isSuffixOf ".txt" f) fs

removeStop :: S.Set String -> [String] -> Bool
removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)
removeStop stopW x = error $ show x
