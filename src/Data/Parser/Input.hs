{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Module    :  Data.Parser.Input
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Files and strings types with positional information.
--
-- @since 0.1.0.0

module Data.Parser.Input
  ( Input(..)
    -- ** Input Lenses
  , str'
  , delta'
  ) where

import           Control.Effect.Lens
import           Control.Effect.State
import           Control.Lens         (Lens', lens)
import           Data.Source

-- | Parser "Input" information.
--
-- @since 0.1.0.0
data Input s = Input
    { position :: {-# UNPACK #-} !Pos
    , inpRange :: {-# UNPACK #-} !Range
    , str      :: ![s]
    }
    deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance HasPos (Input s) where
  pos' = lens position (\s t -> s { position = t })

-- | @since 0.1.0.0
instance HasSpanLike (Input s) Delta where
  start' = lens (rangeStart . inpRange) (\(Input p r s) t -> Input p (r { rangeStart = t}) s)
  end'   = lens (rangeEnd   . inpRange) (\(Input p r s) t -> Input p (r { rangeEnd   = t}) s)

-- | 'str' lens for inputs.
--
-- @since 0.1.0.0
str' :: Lens' (Input s) [s]
str' = lens str (\s t -> s { str = t })
