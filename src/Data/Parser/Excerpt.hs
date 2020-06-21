{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module    :  Data.Parser.Excerpt
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- "Excerpt". Line exerpts for "Err" productions and "Notice"s.
--
-- @since 0.1.0.0

module Data.Parser.Excerpt
  ( Excerpt(..)
  ) where

import           Control.Lens
import           Data.Source
import           Prelude      hiding (span)

-- | Excerpt information: a originating filepath, a line from where the excerpt
-- was raise, and then a span of that line.
--
-- @since 0.1.0.0
data Excerpt = Excerpt
    { excerptPath :: !FilePath
    , excerptLine :: !String
    , excerptSpan :: {-# UNPACK #-} !Span
    }
    deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance HasInterval Excerpt Pos where
  start' = lens (spanStart . excerptSpan) (\(Excerpt p l s) t -> Excerpt p l s { spanStart = t })
  end'   = lens (spanEnd   . excerptSpan) (\(Excerpt p l s) t -> Excerpt p l s { spanEnd   = t })
