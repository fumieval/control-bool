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
-- * Applicative Combinators
, notF
, (<||>)
, (<&&>)
, aguard'
-- * Monadic combinators
, notM
, (<|=>)
, (<&=>)
, guard'
, guardM'
, whenM
, unlessM
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

-- | A lifted 'not'.
notF :: Functor f => f Bool -> f Bool
notF = fmap not

-- | A lifted 'not'.
notM :: Monad m => m Bool -> m Bool
notM = liftM not

-- | A lifted ('&&').
(<&&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&&>) = liftA2 (&&)

-- | A lifted ('&&').
(<&=>) :: Monad m => m Bool -> m Bool -> m Bool
(<&=>) = liftM2 (&&)

-- | A lifted ('||').
(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

-- | A lifted ('||').
(<|=>) :: Monad m => m Bool -> m Bool -> m Bool
(<|=>) = liftM2 (||)

-- | Run the action if the given monadic condition becomes 'True'.
whenM :: (Monoid a, Monad m) => m Bool -> m a -> m a
whenM mp m = mp >>= bool (return mempty) m

-- | Run the action if the given monadic condition becomes 'False'.
unlessM :: (Monoid a, Monad m) => m Bool -> m a -> m a
unlessM mp m = mp >>= bool m (return mempty)

-- | 'guard'' b returns the second argument if b is True, otherwise becomes 'mzero'.
guard' :: MonadPlus m => Bool -> a -> m a
guard' b a = bool mzero (return a) b

-- | 'aguard'' b returns the second argument if b is True, otherwise becomes 'empty'.
aguard' :: Alternative m => Bool -> a -> m a
aguard' b a = bool empty (pure a) b

-- | 'guard'' b returns the second argument if b is True, otherwise becomes mzero.
guardM' :: MonadPlus m => m Bool -> a -> m a
guardM' mp a = mp >>= flip guard' a