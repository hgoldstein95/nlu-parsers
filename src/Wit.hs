module Wit where

import Parsing
import NLP.Hext.NaiveBayes
import Control.Monad.State
import Control.Applicative

keyword :: String -> Parser String String
keyword = somewhereP . stringP'

freeText :: BayesModel Bool -> Parser String String
freeText model = do
  txt <- invisP substringP
  if runBayes model txt then pure txt else empty

intent :: Ord a => BayesModel a -> Parser String a
intent model = runBayes model <$> get
