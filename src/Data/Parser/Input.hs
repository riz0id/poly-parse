{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Data.Parser.Input
  ( Input(..)
    -- ** Input Lenses
  , str'
  , delta'
    -- ** Input Operations
  , advance
  ) where

import Data.Source
import Control.Effect.State
import Control.Lens (Lens', lens)
import Control.Effect.Lens

data Input = Input
  { position :: {-# UNPACK #-} !Pos
  , del      :: {-# UNPACK #-} !Int
  , str      :: !String
  }
  deriving (Eq, Show)

-- | @since 0.1.0.0
instance HasPos Input where
  pos' = lens position (\s t -> s { position = t })

-- | 'del' lens for input deltas
--
-- @since 0.1.0.0
delta' :: Lens' Input Int
delta' = lens del (\s t -> s { del = t })

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
