{-|
The core functionality of <https://wit.ai Wit.ai>, implemented in terms of
'Parser' and its associated combinators.
-}
module Text.NLUParse.Wit (
  -- * Wit Model
    WitModel
  , trainWitModel
  -- * Search Strategies
  , keyword
  , freeText
  , intent
  )
where

import Text.NLUParse

import Control.Monad.State
import Control.Applicative
import qualified Data.Text.Lazy as T

import NLP.Hext.NaiveBayes

-- | A naive Bayes classifier, using the
-- <https://hackage.haskell.org/package/hext hext> library.
type WitModel = BayesModel

-- | Trains a model with pairs of strings and their associated label.
trainWitModel :: Ord label => [(String, label)] -> WitModel label
trainWitModel = foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel

-- | Finds some (case insensitive) keyword, anywhere in the input.
keyword :: String -> Parser String String
keyword = somewhereP . stringP'

-- | Finds a substring of the input that has some semantic meaning.
--
-- Input examples are of the form
-- @(\<potential substring>, \<valid free text>)@.
--
-- /Note:/ Lots of input examples are needed to make this parser work correctly.
freeText :: WitModel Bool -> Parser String String
freeText model = do
  txt <- invisP substringP
  if runBayes model txt then pure txt else empty

-- | Considers the input as a whole and attempts to classify it into some number
-- of categories.
--
-- This can be used to capture things like user intent or mood.
intent :: Ord label => WitModel label -> Parser String label
intent model = runBayes model <$> get
