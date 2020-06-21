{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

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
    -- ** Input Classes
  , HasAdvance(..)
    -- ** Input Lenses
  , input'
    -- ** Input Operations
  ) where

import           Control.Lens (Lens', lens, (^.))
import           Data.Source
import           Data.Text

-- | Parser "Input" information.
--
-- @since 0.1.0.0
data Input s = Input
    { inputPosition :: {-# UNPACK #-} !Pos
    , inputRange    :: {-# UNPACK #-} !Range
    , inputData     :: !s
    }
    deriving (Eq, Ord, Show)

-- | @since 0.1.0.0
instance HasPos (Input s) where
  pos' = lens inputPosition (\s t -> s { inputPosition = t })

-- | @since 0.1.0.0
instance HasRange (Input s) where
  range' = lens inputRange (\s t -> s { inputRange = t })

-- | @since 0.1.0.0
instance HasInterval (Input s) Delta where
  start' = lens (rangeStart . inputRange) (\(Input p r s) t -> Input p (r { rangeStart = t}) s)
  end'   = lens (rangeEnd   . inputRange) (\(Input p r s) t -> Input p (r { rangeEnd   = t}) s)

-- | 'str' lens for inputs.
--
-- @since 0.1.0.0
input' :: Lens' (Input s) s
input' = lens inputData (\s t -> s { inputData = t })

-- | Whether a input can be advanced in a normal way. This includes anything
-- that is nominally a recursive type: Lists, Text, etc...
--
-- @since 0.1.0.0
class HasAdvance a where
  advanceInput :: Input a -> Input a

-- | @since 0.1.0.0
instance HasAdvance String where
  advanceInput i = i
    { inputRange = advanceEnd (i^.range')
    , inputData = case inputData i of
        c : cs -> cs
        []     -> []
    }

-- | @since 0.1.0.0
instance HasAdvance Text where
  advanceInput (Input p r txt) = case uncons txt of
    Just ('\n', ts) -> Input (moveNewline p) (advanceEnd r) ts
    Just (_   , ts) -> Input (moveColumn  p) (advanceEnd r) ts
    Nothing         -> Input p r mempty
  {-# INLINE advanceInput #-}
