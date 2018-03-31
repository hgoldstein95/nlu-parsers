{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Parsing where

import Distribution

import Control.Monad.State
import Control.Applicative
import Data.List (inits, tails)
import qualified Data.Char as Char

newtype Parser s a = Parser { unParser :: StateT s Dist a }
  deriving (Functor, Applicative, Monad, MonadState s)

runParser :: Parser s a -> s -> Dist (a, s)
runParser p s = ($ s) . runStateT . unParser $ p

liftParser :: Dist (a, s) -> Parser s a
liftParser = Parser . StateT . const

instance Alternative (Parser s) where
  empty = liftParser $ dist []
  p <|> q = get >>= \s -> liftParser $
    let res = runParser p s
    in if failed res
       then runParser q s
       else res

uniformP :: [Parser s a] -> Parser s a
uniformP ps = get >>= \s -> liftParser . uniform $ map (`runParser` s) ps

reuniformP :: Parser s a -> Parser s a
reuniformP p = get >>= \s -> liftParser . reuniform $ runParser p s

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

someP :: Parser s a -> Parser s [a]
someP p = (:) <$> p <*> manyP p

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
