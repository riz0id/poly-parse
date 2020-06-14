{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- |
--
-- @since 0.1.0.0

module Control.Effect.Parser
  ( Parser(..), accept, unexpected, delta
  ) where

import           Control.Algebra
import           Data.Kind

data Parser (m :: Type -> Type) k where
  Accept     :: forall s m k. (Char -> Maybe s) -> (s -> k) -> Parser m k
  Unexpected :: String -> Parser m k
  Delta      :: Parser m Int

accept :: Has Parser sig m => (Char -> Maybe s) -> (s -> k) -> m k
accept p f = send (Accept p f)
{-# INLINE accept #-}

unexpected :: Has Parser sig m => String -> m k
unexpected prod = send (Unexpected prod)
{-# INLINE unexpected #-}

delta :: Has Parser sig m => m Int
delta = send Delta
{-# INLINE delta #-}
