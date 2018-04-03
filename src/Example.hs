module Example where

import Control.Applicative (many)

import Parsing
import Wit

newtype Person = Name String deriving (Show)

data Intent
  = Hello
  | Tell Person String
  deriving (Show)

data IntentTag = TellTag | HelloTag deriving (Eq, Ord, Show)

exFreeTextModel :: WitModel Bool
exFreeTextModel =
  trainWitModel
  [ ("to do the dishes", True)
  , ("to go to the movies", True)
  , ("to have a great day", True)
  , ("to go home", True)
  , ("to go to the movies", True)
  , ("tell Harry", False)
  , ("tell Adrian", False)
  , ("tell Harry to", False)
  , ("Harry", False)
  , ("Adrian", False)
  , ("tell", False)
  , ("to", False)
  , ("go", False)
  , ("thing", False)
  ]

exIntentModel :: WitModel IntentTag
exIntentModel =
  trainWitModel
  [ ("hello", HelloTag)
  , ("Hello", HelloTag)
  , ("HELLO", HelloTag)
  , ("tell Adrian to say hi", TellTag)
  , ("tell Harry to do a thing", TellTag)
  , ("tell Someone to go to the park", TellTag)
  ]

witExample :: Parser String (Maybe IntentTag, Maybe Person, Maybe String)
witExample =
  (,,) <$> optionalP (intent exIntentModel)
       <*> optionalP (invisP (anyOffsetP personP))
       <*> optionalP (freeText exFreeTextModel)

witExampleBetter :: Parser String Intent
witExampleBetter = do
  int <- intent exIntentModel
  case int of
    HelloTag -> pure Hello
    TellTag -> Tell <$> invisP (anyOffsetP personP)
                    <*> freeText exFreeTextModel

personP :: Parser String Person
personP = do
  x <- satisfyP (`elem` ['A'..'Z'])
  xs <- many $ satisfyP (/= ' ')
  return $ Name (x : xs)

tellP :: Parser String Intent
tellP = uncurry Tell <$> findMatch2P personP restP <* eofP

helloP :: Parser String Intent
helloP = anyOffsetP (stringP' "hello") >> return Hello

intentP :: Parser String Intent
intentP = uniformP [tellP, helloP] <* eofP
