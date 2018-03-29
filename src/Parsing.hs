{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Parsing where

import Distribution
import Control.Monad.State
import Control.Applicative

import Control.Monad (foldM)
import Data.List (inits, tails)
import qualified Data.Char as Char
import qualified Data.Text.Lazy as T

import NLP.Hext.NaiveBayes

newtype Parser s a = Parser { unParser :: StateT s Dist a }
  deriving (Functor, Applicative, Monad, MonadState s)

runParser :: Parser s a -> s -> Dist (a, s)
runParser p s = ($ s) . runStateT . unParser $ p

liftParser :: Dist (a, s) -> Parser s a
liftParser = Parser . StateT . const

instance Alternative (Parser s) where
  empty = liftParser $ dist []
  p <|> q = get >>= \s -> liftParser $ case runParser p s of
    Dist' [] -> runParser q s
    res -> res

uniformP :: [Parser s a] -> Parser s a
uniformP ps = get >>= \s -> liftParser . uniform $ map (`runParser` s) ps

weightP :: Rational -> Parser s a -> Parser s a -> Parser s a
weightP w p q = get >>= \s ->
  liftParser $ weight w (runParser p s) (runParser q s)

satisfyP :: (a -> Bool) -> Parser [a] a
satisfyP f = get >>= \case
  [] -> empty
  x : xs -> if f x then put xs >> pure x else empty

charP :: Eq a => a -> Parser [a] a
charP c = satisfyP (== c)

charP' :: Char -> Parser String Char
charP' c = satisfyP (\x -> x == Char.toUpper c || x == Char.toLower c)

stringP :: Eq a => [a] -> Parser [a] [a]
stringP = foldr (\c -> (<*>) ((:) <$> charP c)) (pure [])

stringP' :: String -> Parser String String
stringP' = foldr (\c -> (<*>) ((:) <$> charP' c)) (pure [])

manyP :: Parser s a -> Parser s [a]
manyP p = go
  where
    go = go' <|> pure []
    go' = (:) <$> p <*> go

eofP :: Parser [a] ()
eofP = get >>= \case
  [] -> pure ()
  _ -> empty

optionalP :: Parser s a -> Parser s (Maybe a)
optionalP p = Just <$> p <|> pure Nothing

consumeP :: Parser [a] ()
consumeP = do
  s <- get
  (_, rst) <- uniformP $ pure <$> parts s
  put rst
  where
    parts xs = zip (inits xs) (tails xs)

substringP :: Parser [a] [a]
substringP = do
  s <- get
  (_, f, rst) <- uniformP $ pure <$> parts s
  put rst
  pure f
  where
    parts xs =
      [ (x, y, z)
      | (x, ys) <- zip (inits xs) (tails xs)
      , (y, z) <- zip (inits ys) (tails ys)
      ]

anyOffsetP :: Parser [b] a -> Parser [b] a
anyOffsetP p = consumeP >> p

restP :: Parser [a] [a]
restP = get <* put []

findMatch2P :: Parser [c] a -> Parser [c] b -> Parser [c] (a, b)
findMatch2P p q = (,) <$> anyOffsetP p <*> anyOffsetP q

invisP :: Parser s a -> Parser s a
invisP p = do
  s <- get
  x <- p
  put s
  pure x

somewhereP :: Parser [b] a -> Parser [b] a
somewhereP = invisP . anyOffsetP


-- * Wit

keyword :: String -> Parser String String
keyword = somewhereP . stringP'

freeText :: BayesModel Bool -> Parser String String
freeText model = do
  txt <- invisP substringP
  if runBayes model txt then pure txt else empty

intent :: Ord a => BayesModel a -> Parser String a
intent model = runBayes model <$> get


-- * Example

newtype Person = Name String deriving (Show)

data Intent
  = Hello
  | Tell Person String
  deriving (Show)

data IntentTag = TellTag | HelloTag deriving (Eq, Ord, Show)

exFreeTextModel :: BayesModel Bool
exFreeTextModel =
  foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel
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
  , ("tell Harry", False)
  , ("to", False)
  , ("go", False)
  , ("thing", False)
  ]

exIntentModel :: BayesModel IntentTag
exIntentModel =
  foldl (\m (sample, cl) -> teach (T.pack sample) cl m) emptyModel
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
  xs <- manyP $ satisfyP (/= ' ')
  return $ Name (x : xs)

tellP :: Parser String Intent
tellP = uncurry Tell <$> findMatch2P personP restP <* eofP

helloP :: Parser String Intent
helloP = anyOffsetP (stringP' "hello") >> return Hello

intentP :: Parser String Intent
intentP = uniformP [tellP, helloP] <* eofP
