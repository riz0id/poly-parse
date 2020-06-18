
-- | Module    :  Data.Parser.Err
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Err. Error production information.
--
-- @since 0.1.0.0

module Data.Parser.Err
  ( Err(..)
    -- ** Err Lenses
  , reason', expected'
    -- ** Errs
  , emitPlainErr
  , errToNotice
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.Parser.Excerpt
import           Data.Parser.Input
import           Data.Parser.Notice
import           Data.Set
import           Data.Text
import           Data.Text.Prettyprint.Doc                 (Doc, pretty)
import Data.Source
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

-- | Error production information.
--
-- @since 0.1.0.0
data Err = Err
    { errInput    :: Input
    , errReason   :: Maybe (Doc AnsiStyle)
    , errExpected :: Set Text
    }

-- | Lens focusing on "Err.errReason".
--
-- @since 0.1.0.0
reason' :: Lens' Err (Maybe (Doc AnsiStyle))
reason' = lens errReason (\s t -> s { errReason = t })
{-# INLINE reason' #-}

-- | Lens focusing on "Err.errExpected".
--
-- @since 0.1.0.0
expected' :: Lens' Err (Set Text)
expected' = lens errExpected (\s t -> s { errExpected = t })
{-# INLINE expected' #-}

-- | Emit an ordinary error message.
--
-- >> show (emitPlainErr "unexpected input")
-- >> error: unexpected input
--
-- @since 0.1.0.0
emitPlainErr :: Doc AnsiStyle -> Err
emitPlainErr msg = Err (Input emptyPos mempty []) (Just msg) mempty
{-# INLINE CONLIKE emitPlainErr #-}

-- | Injection from Error type into a Notice.
--
-- @since 0.1.0.0
errToNotice :: FilePath -> Err -> Notice
errToNotice fp err = Notice
  { noticeLevel   = Just Error
  , noticeExcerpt = Excerpt
    { excerptPath = fp
    , excerptLine = mempty
    , excerptSpan = Span emptyPos emptyPos
    }
  , noticeReason  = fromMaybe (pretty "unknown error") (err^.reason')
  , noticeContext = []
  }
