
-- | Module    :  Data.Parser.Notice
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Notice information for error productions.
--
-- @since 0.1.0.0

module Data.Parser.Notice
  ( Notice(..), Level(..)
  ) where

import           Data.Parser.Excerpt
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

-- | A production thrown by a parser. Informed by the level of the notice, an
-- excerpt of the issue(typically the line where the issue occured), a reason,
-- and the relevant context for that issue.
--
-- @since 0.1.0.0
data Notice = Notice
    { noticeLevel   :: !(Maybe Level)
    , noticeExcerpt :: {-# UNPACK #-} !Excerpt
    , noticeReason  :: !(Doc AnsiStyle)
    , noticeContext :: ![Doc AnsiStyle]
    }

-- | Levels of productions a paser can raise.
--
-- @since 0.1.0.0
data Level = Warn
    | Error
    deriving Enum
