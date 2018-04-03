{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

{-|
This module provides a monadic interface for dealing with multinomial
distributions and failure. The 'Dist' monad captures the idea of partial
computations with probabilistic dependencies
-}
module Distribution (
  -- * Dist
    Dist
  , runDist
  , dist
  -- * Distribution Combinators
  , failed
  , collapse
  , uniform
  , reuniform
  , flatten
  , weight
  )
where

import Control.Arrow (second)
import qualified Data.Map as Map

-- | A possibly empty multinomail distribution.
--
-- Instances of this type are required to maintain the "distribution invariant":
--
-- prop> runDist xs == [] || sum (snd <$> runDist xs) == 1
--
-- Intuitively, the invariant states that a distribution has either failed, i.e.
-- the list is empty, or its probabilities sum to @1@. The most common way to
-- maintain the invariant is to construct all distributions with 'dist', which
-- maintains the invariant by construction.
newtype Dist a = Dist'
  { runDist :: [(a, Rational)] -- ^ Extracts the underlying distribution from a
                               -- 'Dist'.
  } deriving (Functor)

instance Show a => Show (Dist a) where
  show = show . runDist

-- | Builds a distribution from an association of outcomes to probabilities. Any
-- 'Dist' created by this function obeys the required invariant.
--
-- It is worth noting that this constructor does not ensure that values in the
-- distribution are unique. If this property is required, see 'collapse'.
dist :: [(a, Rational)] -> Dist a
dist xs =
  let s = sum (snd <$> xs)
  in Dist' $ second (/ s) <$> filter ((/= 0) . snd) xs

-- | Collapses equal elements in a distribution. While it is not technically
-- incorrect to have a distribution like,
--
-- > [("hello", 1 % 2), ("world", 1 % 4), ("hello", 1 % 4)]
--
-- sometimes it is undesirable. Using 'collapse', we can transform the above
-- distribution into:
--
-- > [("hello", 3 % 4), ("world", 1 % 4)]
collapse :: Ord a => Dist a -> Dist a
collapse d = dist $ Map.toList $ go Map.empty (runDist d)
  where
    go acc [] = acc
    go acc ((x, p) : xs) =
      case Map.lookup x acc of
        Nothing -> go (Map.insert x p acc) xs
        Just q -> go (Map.insert x (p + q) acc) xs

instance Applicative Dist where
  pure x = dist [(x, 1)]
  fM <*> xM = dist
    [ (f x, p * q)
    | (f, p) <- runDist fM
    , (x, q) <- runDist xM
    ]

instance Monad Dist where
  return = pure
  xM >>= f = dist
    [ (res, p * q)
    | (x, p) <- runDist xM
    , (res, q) <- runDist (f x)
    ]
  fail _ = dist []

-- | Creates a distribution with each element weighted equally.
--
-- For example:
--
-- >>> uniform [1, 2, 3]
-- [(1, 1 % 3), (2, 1 % 3), (3, 1 % 3)]
uniform :: [a] -> Dist a
uniform as =
  dist $ (, 1) <$> as

-- | Turns any distribution into a uniform one. The probability of each element
-- will be @1 % n@, where @n@ is the total number of elements in the
-- distribution.
reuniform :: Dist a -> Dist a
reuniform d = dist $ second (const 1) <$> runDist d

-- | Combines a list of distributions, weighting the contribution of each
-- uniformly.
--
-- For example:
--
-- >>> flatten [uniform [1, 2], pure 3]
-- [(1, 1 % 4), (2, 1 % 4), (3, 1 % 2)]
flatten :: [Dist a] -> Dist a
flatten as =
  let w = 1 / fromIntegral (length as)
  in dist $ fmap (second (* w)) . runDist =<< as

-- | Combines a two distributions with a given weighting.
--
-- For example:
--
-- >>> weight 0.1 (pure 1) (pure 2)
-- [(1, 1 % 10), (2, 9 % 10)]
--
-- and
--
-- >>> weight 0.1 (pure 1) (uniform [2, 3, 4])
-- [(1, 1 % 10), (2, 3 % 10), (3, 3 % 10), (4, 3 % 10)]
weight
  :: Rational -- ^ the weight of the first distribution, between @0@ and @1@
  -> Dist a   -- ^ first distribution
  -> Dist a   -- ^ second distribution
  -> Dist a
weight w x y =
  let x' = second (* w) <$> runDist x
      y' = second (* (1 - w)) <$> runDist y
  in dist $ x' ++ y'

-- | Checks if a distribution has failed, i.e. 'runDist' returns @[]@.
failed :: Dist a -> Bool
failed = null . runDist
