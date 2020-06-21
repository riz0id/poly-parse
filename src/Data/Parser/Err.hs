
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
  , prettyColorErr
  , emitPlainErr
  , errToNotice
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.Parser.Excerpt
import           Data.Parser.Input
import           Data.Parser.Notice
import           Data.Set
import           Data.Source
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle,
                                                            Color (..), color)

-- | Error production information.
--
-- @since 0.1.0.0
data Err s = Err
    { errInput    :: Input s
    , errReason   :: Maybe (Doc AnsiStyle)
    , errExpected :: Set Text
    }
    deriving Show

instance Pretty s => Pretty (Err s) where
  pretty err = pretty "error:" <+> vsep (fmap pretty . toList $ err^.expected')

-- | Colored version for a pretty error
--
-- @since 0.1.0.0
prettyColorErr :: Err s -> Doc AnsiStyle
prettyColorErr err = annotate (color Red) (pretty "error:")
                 <+> vsep (fmap pretty . toList $ err^.expected')

-- | Lens focusing on "Err.errReason".
--
-- @since 0.1.0.0
reason' :: Lens' (Err s) (Maybe (Doc AnsiStyle))
reason' = lens errReason (\s t -> s { errReason = t })
{-# INLINE reason' #-}

-- | Lens focusing on "Err.errExpected".
--
-- @since 0.1.0.0
expected' :: Lens' (Err s) (Set Text)
expected' = lens errExpected (\s t -> s { errExpected = t })
{-# INLINE expected' #-}

-- | Emit an ordinary error message.
--
-- >> show (emitPlainErr "unexpected input")
-- >> error: unexpected input
--
-- @since 0.1.0.0
emitPlainErr :: Monoid s => Doc AnsiStyle -> (Err s)
emitPlainErr msg = Err (Input emptyPos mempty mempty) (Just msg) mempty
{-# INLINE CONLIKE emitPlainErr #-}

-- | Injection from Error type into a Notice.
--
-- @since 0.1.0.0
errToNotice :: FilePath -> Err s -> Notice
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
