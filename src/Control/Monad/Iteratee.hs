{-# LANGUAGE DeriveFunctor #-}

module Control.Monad.Iteratee where

import           Control.Comonad
import           Control.Monad
import           Data.Profunctor

-- |
--
-- @since 0.1.0.0
data Iteratee r a
  = Iteratee a (r -> Iteratee r a)
  | Done a
  deriving Functor

-- | Feed a value to 'Iteratee', obtaining a new (partial or final) result.
--
-- @since 0.1.0.0
simplifyIt :: Iteratee r a -> r -> Iteratee r a
simplifyIt (Iteratee _ k) r = k r
simplifyIt pa             _ = pa
{-# INLINE simplifyIt #-}

-- | Extracts the tail result from a iteratee
--
-- @since 0.1.0.0
indexIt :: Iteratee r a -> r -> a
indexIt (Done     a)   _ = a
indexIt (Iteratee _ k) r = extract (k r)
{-# INLINE indexIt #-}

-- | @since 0.1.0.0
instance Applicative (Iteratee r) where
  pure = Done
  {-# INLINE CONLIKE pure #-}

  Done f       <*> Done x       = Done     (f x)
  Done f       <*> Iteratee x c = Iteratee (f x) (fmap f . c)
  Iteratee f c <*> Done x       = Iteratee (f x) (fmap ($ x) . c)
  Iteratee f d <*> Iteratee x c = Iteratee (f x) (\r -> d r <*> c r)
  {-# INLINEABLE (<*>) #-}

-- | @since 0.1.0.0
instance Profunctor Iteratee where
  rmap = fmap
  {-# INLINE rmap #-}

  lmap _ (Done x)       = Done x
  lmap f (Iteratee a g) = Iteratee a (lmap f . g . f)
  {-# INLINEABLE lmap #-}

-- | @since 0.1.0.0
instance ComonadApply (Iteratee r) where
  (<@>) = (<*>)
  {-# INLINE (<@>) #-}

-- | @since 0.1.0.0
instance Comonad (Iteratee r) where
  duplicate p@Done{}         = Done p
  duplicate p@(Iteratee _ k) = Iteratee p (duplicate . k)
  {-# INLINEABLE duplicate #-}

  extend f p@Done{}         = Done (f p)
  extend f p@(Iteratee _ k) = Iteratee (f p) (extend f . k)
  {-# INLINEABLE extend #-}

  extract (Done x)       = x
  extract (Iteratee x _) = x
  {-# INLINE extract #-}

-- | @since 0.1.0.0
instance Monad (Iteratee r) where
  return = pure
  {-# INLINE CONLIKE return #-}

  Done     a   >>= f = f a
  Iteratee a k >>= f = Iteratee (extract (f a)) $ \r -> case k r of
    Iteratee a' k' -> Iteratee (indexIt (f a') r) $ k' >=> f
    Done     a'    -> simplifyIt (f a') r
  {-# INLINEABLE (>>=) #-}
