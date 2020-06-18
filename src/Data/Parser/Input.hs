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
    -- ** Input Operations
  , advance
  ) where

import           Control.Effect.Lens
import           Control.Effect.State
import           Control.Lens         (Lens', lens)
import           Data.Source

-- | Parser "Input" information.
--
-- @since 0.1.0.0
data Input = Input
    { position :: {-# UNPACK #-} !Pos
    , inpRange :: {-# UNPACK #-} !Range
    , str      :: !String
    }
    deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance HasPos Input where
  pos' = lens position (\s t -> s { position = t })

-- | @since 0.1.0.0
instance HasSpanLike Input Delta where
  start' = lens (rangeStart . inpRange) (\(Input p r s) t -> Input p (r { rangeStart = t}) s)
  end'   = lens (rangeEnd   . inpRange) (\(Input p r s) t -> Input p (r { rangeEnd   = t}) s)

-- | 'str' lens for inputs.
--
-- @since 0.1.0.0
str' :: Lens' Input String
str' = lens str (\s t -> s { str = t })

-- | Advances the current input forward by a character. We keep track of the
-- lines and columns by checking if the input points two a newline character.
--
-- In the case that we aren't at a newline, we just increment the column.
--
-- @since 0.1.0.0
advance :: Has (State Input) sig m => m Input
advance = use str' >>= \case
  '\n' : cs -> do
    line'   @Input .= 0
    column' @Input += 1
    str'           .= cs
    get

  _    : cs -> do
    line'  @Input += 1
    str'          .= cs
    get

  _         -> get
