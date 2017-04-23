{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Applicative instance to use with applicative converter.
module Metamorphosis.Applicative where

import Control.Applicative
import Data.Functor.Identity

-- | Extract
class Extract a f b where
  extract :: a -> f b

instance Applicative f => Extract a f a where
  extract  = pure
instance Extract [a] [] (Identity a) where
  extract  = map pure
instance Applicative f => Extract (f a) f a where
  extract  = id
instance Applicative f => Extract a Identity (f a) where
  extract = pure . pure
instance Extract () Maybe a where
  extract () = Nothing
instance Monoid a => Extract () Identity a where
  extract () = Identity mempty
instance Extract (Maybe a) Identity [a] where
  extract Nothing = Identity []
  extract (Just x) = Identity [x]

instance Extract ([a]) ZipList a where
  extract = ZipList

instance (Applicative f, Extract a f a', Extract b f b') => Extract (a, b) f (a',b') where
  extract (a,b) = (,) <$> extract a <*> extract b
instance (Applicative f, Extract a f a', Extract b f b', Extract c f c') => Extract (a, b, c) f (a',b', c') where
  extract (a,b,c) = (,,) <$> extract a <*> extract b <*> extract c
