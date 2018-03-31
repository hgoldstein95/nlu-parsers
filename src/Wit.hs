module Wit where

import Parsing

import Control.Monad.State
import Control.Applicative
import qualified Data.Text.Lazy as T

import NLP.Hext.NaiveBayes

type WitModel = BayesModel

trainWitModel :: Ord a => [(String, a)] -> WitModel a
trainWitModel = foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel

keyword :: String -> Parser String String
keyword = somewhereP . stringP'

freeText :: BayesModel Bool -> Parser String String
freeText model = do
  txt <- invisP substringP
  if runBayes model txt then pure txt else empty

intent :: Ord a => BayesModel a -> Parser String a
intent model = runBayes model <$> get
