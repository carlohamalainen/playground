{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Prob where

-- http://jtobin.ca/encoding-independence-statically

import Control.Applicative.Free
import Control.Monad
import Control.Monad.Free
import Control.Monad.Primitive
import System.Random.MWC.Probability (Prob)
import qualified System.Random.MWC.Probability as MWC


data ProbF r = BetaF      Double Double (Double -> r)
             | BernoulliF Double        (Bool   -> r)
             | FlabertF   Double Double (Double -> r)
             | BloopF (Double -> Double) Double Double (Double -> r)
             deriving Functor

type Model = Free ProbF

beta :: Double -> Double -> Model Double
beta a b = liftF $ BetaF a b id

bernoulli :: Double -> Model Bool
bernoulli p = liftF $ BernoulliF p id

flabert :: Double -> Double -> Model Double
flabert a b = liftF $ FlabertF a b id

bloop :: (Double -> Double) -> Double -> Double -> Model Double
bloop fn a b = liftF $ BloopF fn a b id

coin :: Double -> Double -> Model Bool
coin a b = do
    x <- beta a b
    bernoulli x

eval = iterM $ \case
    BetaF a b k     -> MWC.beta a b    >>= k
    BernoulliF p k  -> MWC.bernoulli p >>= k
    FlabertF a b k  -> do z <- MWC.beta a b
                          let z' = if z < 0 then 2*z else 3*z
                          k z'
    BloopF fn a b k  -> (fn <$> MWC.beta a b) >>= k

coin' :: Double -> Double -> Model Bool
coin' a b = do
    x <- flabert a b
    bernoulli x

coin'' :: (Double -> Double) -> Double -> Double -> Model Bool
coin'' fn a b = do
    x <- bloop fn a b
    bernoulli x

go1 :: IO ()
go1 = do
    gen <- MWC.createSystemRandom
    replicateM 10 (MWC.sample (eval $ coin 1 1) gen) >>= print

go2 :: IO ()
go2 = do
    gen <- MWC.createSystemRandom
    replicateM 10 (MWC.sample (eval $ coin' 1 1) gen) >>= print

go3 :: IO ()
go3 = do
    gen <- MWC.createSystemRandom
    replicateM 10 (MWC.sample (eval $ coin'' (+100) 1 1) gen) >>= print






