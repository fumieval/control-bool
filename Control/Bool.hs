-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Bool
-- Copyright   :  (C) 2013 Fumiaki Kinoshita
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Fumiaki Kinoshita <fumiexcel@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Useful combinators for boolean expressions
----------------------------------------------------------------------------

module Control.Bool (
-- * A pure combinator
bool
-- * Applicative combinators
, notF
, (<||>)
, (<&&>)
, aguard
, aguard'
-- * Monadic combinators
, notM
, (<|=>)
, (<&=>)
, guard'
, guardM'
, whenM
, unlessM
, ifThenElseM
) where

import Control.Applicative
import Control.Monad
import Data.Monoid

infixr 3 <&&>, <&=>
infixr 2 <||>, <|=>

-- | Return its second argument if the boolean value is 'True', otherwise return first.
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ y True = y
{-# INLINE bool #-}

-- | A lifted 'not'.
notF :: Functor f => f Bool -> f Bool
notF = fmap not
{-# INLINE notF #-}

-- | A lifted 'not'.
notM :: Monad m => m Bool -> m Bool
notM = liftM not
{-# INLINE notM #-}

-- | A lifted ('&&').
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)
{-# INLINE (<&&>) #-}

-- | A lifted ('&&'), but it doesn't run the second argument if the first returns False.
(<&=>) :: Monad m => m Bool -> m Bool -> m Bool
m <&=> n = do
    r <- m
    if r then n else return False
{-# INLINE (<&=>) #-}

-- | A lifted ('||').
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
{-# INLINE (<||>) #-}

-- | A lifted ('||'), but it doesn't run the second argument if the first returns True.
(<|=>) :: Monad m => m Bool -> m Bool -> m Bool
m <|=> n = do
    r <- m
    if r then return True else n
{-# INLINE (<|=>) #-}

-- | Run the action if the given monadic condition becomes 'True'.
whenM :: (Monoid a, Monad m) => m Bool -> m a -> m a
whenM mp m = mp >>= bool (return mempty) m
{-# INLINE whenM #-}

-- | Run the action if the given monadic condition becomes 'False'.
unlessM :: (Monoid a, Monad m) => m Bool -> m a -> m a
unlessM mp m = mp >>= bool m (return mempty)
{-# INLINE unlessM #-}

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM mp m n = do
    r <- mp
    if r then m else n
{-# INLINE ifThenElseM #-}

-- | 'guard'' b returns the second argument if b is True, otherwise becomes 'mzero'.
guard' :: MonadPlus m => Bool -> a -> m a
guard' b a = bool mzero (return a) b
{-# INLINE guard' #-}

-- | 'guard'' b returns the second argument if b is True, otherwise becomes mzero.
guardM' :: MonadPlus m => m Bool -> a -> m a
guardM' mp a = mp >>= flip guard' a
{-# INLINE guardM' #-}

-- | An 'Alternative' analogue of 'guard'.
aguard :: Alternative m => Bool -> m ()
aguard = bool empty (pure ())
{-# INLINE aguard #-}

-- | 'aguard'' b returns the second argument if b is True, otherwise becomes 'empty'.
aguard' :: Alternative m => Bool -> a -> m a
aguard' b a = bool empty (pure a) b
{-# INLINE aguard' #-}
