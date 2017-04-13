{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE InstanceSigs #-}
-- Implementation of LogicM based on two-continuation model of streams

{- Copyright (c) 2005, Amr Sabry, Chung-chieh Shan, Oleg Kiselyov,
	and Daniel P. Friedman
-}

module SFK1 (
  SG, runM
) where

import Control.Monad
import LogicM
import GHC.Base

type SG a = SFK a

newtype SFK a = SFK { unSFK :: forall ans. SK ans a -> FK ans -> ans }

type FK ans = ans
type SK ans a = a -> FK ans -> ans

instance Functor SFK where
  fmap :: (a -> b) -> SFK a -> SFK b
  -- fmap f sfk = SFK (\ sk fk -> unSFK sfk (\x fk -> sk (f x) fk) fk)
  fmap f (SFK m) = SFK (\ sk -> m (sk . f))

instance Monad SFK where
  return e = SFK (\sk fk -> sk e fk)
  (SFK m) >>= f =
      SFK (\sk fk ->
           m (\a fk' -> unSFK (f a) sk fk')
             fk)

instance Applicative SFK where
  pure :: a -> SFK a
  pure x = return x

  (<*>) :: SFK (a -> b) -> SFK a -> SFK b
  (<*>) = ap

instance MonadPlus SFK where
  mzero :: SFK a
  mzero = SFK (\_ fk -> fk)
  mplus :: SFK a -> SFK a -> SFK a
  (SFK m1) `mplus` (SFK m2) = SFK (\sk fk -> m1 sk (m2 sk fk))

instance Alternative SFK where
  empty :: SFK a
  empty = mzero
  (<|>) :: SFK a -> SFK a -> SFK a
  (<|>) = mplus

instance LogicM SFK where
  msplit (SFK m) = m ssk caseB
    where caseB = return $ Nothing
          caseA h t = return $ Just (h, t)
          ssk sub fk = caseA
            sub
            (do r <- fk
                case r of
                  Nothing -> mzero
                  Just (sg1, sgr) -> (return sg1) `mplus` sgr)

runM :: SG a -> [a]
runM (SFK m) = m (:) []
