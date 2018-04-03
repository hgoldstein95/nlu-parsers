{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-|
We provide a probabilistic parsing framework, as well as a number of useful
parser combinators. Our hope is to provide all of the basic combinators that one
would need to replicate a standard NLU system. See 'Wit' for an example.
-}
module Parsing (
  -- * Parser
    Parser(..)
  , runParser
  , liftParser
  -- * Distribution Combinators
  , uniformP
  , reuniformP
  , noCollapseReuniformP
  , weightP
  -- * Standard Parser Combinators
  , satisfyP
  , charP
  , charP'
  , stringP
  , stringP'
  , eofP
  , optionalP
  , invisP
  , restP
  -- * Combinatorial Parsers
  , consumeP
  , substringP
  , anyOffsetP
  , findMatch2P
  , somewhereP
  )
where

import Control.Monad.State
import Control.Applicative
import Data.List (inits, tails)
import qualified Data.Char as Char

import Distribution

-- | The parser type. This is similar to the construction used by libraries like
-- 'Parsec', but it produces a probability distribution of results.
newtype Parser s a = Parser { unParser :: StateT s Dist a }
  deriving (Functor, Applicative, Monad, MonadState s)

-- | Runs a parser on a given input, and produces a distribution on pairs of
-- possible parse results and the associated remaining strings.
runParser :: Parser s a -> s -> Dist (a, s)
runParser p s = ($ s) . runStateT . unParser $ p

-- | Lifts a distribution over parse results and remainders into the 'Parser'
-- monad.
liftParser :: Dist (a, s) -> Parser s a
liftParser = Parser . StateT . const

instance Alternative (Parser s) where
  empty = liftParser $ dist []
  p <|> q = get >>= \s -> liftParser $
    let res = runParser p s
    in if failed res
       then runParser q s
       else res

-- | Combines a list of parsers with their results weighted uniformly. If no
-- more than one of the parsers succeed, this is equivalent to
--
-- > foldr (<|>) empty
--
-- otherwise, all of the parsers that succeed are treated as equally likely.
uniformP :: [Parser s a] -> Parser s a
uniformP ps = get >>= \s -> liftParser . flatten $ map (`runParser` s) ps

-- | Retroactively modifies the probabilities of parse results to enforce that
-- every result is treated as equally likely. In order to do this correctly, we
-- require that both @s@ and @a@ be @Ord@, which ensures that a distribution
-- like
--
-- > [(("hello", ""), 1 % 4), (("hello", ""), 1 % 2), (("world", ""), 1 % 4)]
--
-- becomes
--
-- > [(("hello", ""), 1 % 2), (("world", ""), 1 % 2)]
--
-- instead of
--
-- > [(("hello", ""), 1 % 3), (("hello", ""), 1 % 3), (("world", ""), 1 % 3)]
--
-- If @Ord@ instances cannot be produced, see 'noCollapseReuniformP'.
reuniformP :: (Ord s, Ord a) => Parser s a -> Parser s a
reuniformP p = get >>= \s -> liftParser . reuniform . collapse $ runParser p s

-- | This function is the same as 'reuniformP', but it does not collapse equal
-- elements.
--
-- __Warning:__ If @s@ and @a@ are instances of @Ord@, you likely want to use
-- 'reuniformP'.
noCollapseReuniformP :: Parser s a -> Parser s a
noCollapseReuniformP p = get >>= \s -> liftParser . reuniform $ runParser p s

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
