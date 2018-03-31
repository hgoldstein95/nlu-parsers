{-# LANGUAGE DeriveFunctor #-}

module Distribution
  ( Dist
  , runDist
  , dist
  , uniform
  , weight
  , failed
  )
where

import Data.List (sort)
import Control.Arrow (second)

newtype Dist a = Dist' { runDist :: [(a, Rational)] } deriving (Functor, Show)

dist :: [(a, Rational)] -> Dist a
dist xs =
  let s = sum (snd <$> xs)
  in Dist' $ second (/ s) <$> filter ((/= 0) . snd) xs

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

uniform :: [Dist a] -> Dist a
uniform as =
  let w = 1 / fromIntegral (length as)
  in dist $ fmap (second (* w)) . runDist =<< as

weight :: Rational -> Dist a -> Dist a -> Dist a
weight w x y =
  let x' = second (* w) <$> runDist x
      y' = second (* (1 - w)) <$> runDist y
  in dist $ x' ++ y'

failed :: Dist a -> Bool
failed = null . runDist
