{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Module    :  Control.Effect.Parser.Internal
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Internal syntax of the parser effect.
--
-- @since 0.1.0.0

module Control.Effect.Parser.Internal
  ( -- * Parser Effect
    Parser(..)
    -- ** Effects
  , satisfy
  , unexpected
  ) where

import           Control.Algebra
import           Data.Text

-- | Syntax for a parser effect.
--
-- @since 0.1.0.0
data Parser m k where
  Satisfy    :: (Char -> Maybe a) -> (a -> m k) -> Parser m k
  Unexpected :: Text -> m k -> Parser m k

-- | Satisfication of a predicate coupled with a transformation on the result
-- type of the predicate.
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
satisfy :: Has Parser sig m => (Char -> Maybe a) -> (a -> m k) -> m k
satisfy p f = send (Satisfy p f)
{-# INLINE CONLIKE satisfy #-}

-- | Throw a parser failure given a message for the error production.
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
unexpected :: Has Parser sig m => Text -> m k -> m k
unexpected msg parser = send (Unexpected msg parser)
{-# INLINE CONLIKE unexpected #-}
