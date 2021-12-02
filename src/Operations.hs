module Operations
    (
      filterPOS,lemmatize
    ) where

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


-- todo:後でPOSでフィルターをかける
-- ここで品詞分解を使いたい
posPat = ["N","AN","NN","X","NSN","V"]
filterPOS :: [String] -> Bool
filterPOS phr = True

--　todo:見出し語にする
lemmatize :: [String] ->　[String]
lemmatize a = a


       