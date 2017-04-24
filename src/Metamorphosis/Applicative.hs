{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- | Applicative instance to use with applicative converter.
module Metamorphosis.Applicative where

import Control.Applicative
import Data.Functor.Identity

-- | Convert a value within an applicative functor.
-- Used to convert between applicatives (ex Maybe to [])
-- when using metamorphosis generated converters.
-- Allows as well to fill unknown values in converters
-- by converting () to mempty when possible.
class ConvertA a f b where
  convertA :: a -> f b

instance Applicative f => ConvertA a f a where
  convertA  = pure
instance ConvertA [a] [] (Identity a) where
  convertA  = map pure
instance Applicative f => ConvertA (f a) f a where
  convertA  = id
instance Applicative f => ConvertA a Identity (f a) where
  convertA = pure . pure
instance ConvertA () Maybe a where
  convertA () = Nothing
instance ConvertA () Identity (Maybe a) where
  convertA () = Identity (Nothing)
-- instance Monoid a => ConvertA () Identity a where
  -- convertA () = Identity mempty
instance ConvertA () Identity [a] where
  convertA () = Identity []
instance ConvertA (Maybe a) Identity [a] where
  convertA Nothing = Identity []
  convertA (Just x) = Identity [x]

instance ConvertA ([a]) ZipList a where
  convertA = ZipList

instance (Applicative f, ConvertA a f a', ConvertA b f b') => ConvertA (a, b) f (a',b') where
  convertA (a,b) = (,) <$> convertA a <*> convertA b
instance (Applicative f, ConvertA a f a', ConvertA b f b', ConvertA c f c') => ConvertA (a, b, c) f (a',b', c') where
  convertA (a,b,c) = (,,) <$> convertA a <*> convertA b <*> convertA c
