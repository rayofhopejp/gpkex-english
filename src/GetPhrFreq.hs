module GetPhrFreq
    (
      runGetPhrFreq
    ) where

import Config
import Operations
import Data.Char
import Data.List
import System.IO
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory
import NLP.POS


stemN = 5 :: Int
takeNumber = 100000 :: Int -- default:960

takeTuple :: [String] -> [[String]]
takeTuple [] = []
takeTuple w
 | l >= 4    = [take 4 w] ++ [take 3 w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l == 3    = [w] ++ [take 2 w] ++ [[head w]] ++ (takeTuple $ tail w)
 | l >= 2    = [w] ++ [[head w]] ++ [tail w]
 | otherwise = [w]
  where l = length w

-- ,.!とかで区切って小文字にする(問題なし)
splitText :: String -> String -> [String]
splitText word []     = [word]
splitText word (t:ts)
  | not (isAlpha t || isSpace t || isDigit t || elem t "-'\"") = [word] ++ splitText "" ts
  | isDigit t || elem t "-'\"" = splitText word ts
  | isSpace t = splitText (word ++ " ") ts
  | otherwise = splitText (word ++ [toLower t]) ts

-- "This is Text. Hello, boys" -> [["this","is","text"],["this","is"],["this"],["is","text"],["is"],["text"],["hello"],["boys"]]
makeTuples :: String -> [[String]]
makeTuples = concat . map takeTuple . map words . splitText "" . filter (\x -> not (elem x "€—£§«»<@♦¬°►•[_{„¥©>^~■®▼]"))

--krati riječ
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

getWordFreq :: [[String]] -> M.Map [String] Int
getWordFreq = foldl (\map key -> M.insert (stem key) 1 map) M.empty

filterFiles [] = error "empty folder"
filterFiles fs = map (\f -> inDir ++ f) $ sort $ filter (\f -> isSuffixOf ".txt" f) fs

getFileText stopW m' fn = do
  s <- readFile fn -- "this is document !"とかを読む
  m <- m' --多分これは変えないですむ
  let nm = getWordFreq $ map (\ph -> lemmatize ph) $ filter (\ph -> filterPOS ph) $ filter (removeStop stopW) $ makeTuples $ s
  return $ M.unionWith (+) m nm

-- main
runGetPhrFreq :: IO ()
runGetPhrFreq = do
  sws <- readFile stopWordFile
  let stopW = S.fromList $ lines sws -- stopwordsのsetを作る
  fns <- getDirectoryContents inDir
  -- xmlファイルのみを抽出
  let a = take takeNumber $ filterFiles fns
  let b = foldl (getFileText stopW) emptyMap a
  b' <- b
  writeFile candidateFrequency $ show $ M.toList b'

emptyMap :: IO (M.Map [String] Int)
emptyMap = return M.empty


removeStop stopW (a:[]) = not (S.member a stopW)
removeStop stopW (a:b:[]) = not (S.member a stopW) && not (S.member b stopW)
removeStop stopW (a:b:c:[]) = not (S.member a stopW) && not (S.member c stopW)
removeStop stopW (a:b:c:d:[]) = not (S.member a stopW) && not (S.member d stopW)
