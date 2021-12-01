module Lib
    ( someFunc,
      someFunc2,
    ) where

import GenProg
import Data.Generics
import Control.Monad
import Control.Monad.Random
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import System.Directory
import NLP.POS

someFunc :: IO ()
someFunc = do
  tagger <- defaultTagger
  print $ tagStr tagger "This is a sample sentence."

someFunc2 :: IO ()
someFunc2 = putStrLn "someFunc2"
