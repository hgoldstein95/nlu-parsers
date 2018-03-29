{-# LANGUAGE DeriveFunctor #-}

module Distribution where

import Control.Lens (toListOf, _2, over)
import Data.List (sort)

newtype Dist a = Dist' { runDist :: [(a, Rational)] } deriving (Functor, Show)

dist :: [(a, Rational)] -> Dist a
dist xs =
  let s = sum $ toListOf (traverse._2) xs
  in Dist' $ over (traverse._2) (* (1/s)) xs

instance Applicative Dist where
  pure x = dist [(x, 1)]
  fM <*> xM = dist
    [ (f x, p * q)
    | (f, p) <- runDist fM
    , (x, q) <- runDist xM
    ]

instance Monad Dist where
  return = pure
  xM >>= f = dist $ do
    (x, p) <- runDist xM
    (res, q) <- runDist $ f x
    [(res, p * q)]

uniform :: [Dist a] -> Dist a
uniform as = dist $ do
  a <- as
  over (traverse._2) (* w) (runDist a)
  where
    w = 1 / fromIntegral (length as)

weight :: Rational -> Dist a -> Dist a -> Dist a
weight w x y =
  let modAll = over (traverse._2)
      x' = modAll (* w) (runDist x)
      y' = modAll (* (1 - w)) (runDist y)
  in dist $ x' ++ y'
