
-- |
--
-- @since 0.1.0.0

module Data.Parser.Err
  ( Err(..)
    -- ** Err Lenses
  , reason', footnotes', expected'
  , emitPlainErr
  ) where

import           Control.Lens
import           Data.Set
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)

data Err = Err
    { reason    :: Maybe (Doc AnsiStyle)
    , footnotes :: [Doc AnsiStyle]
    , expected  :: Set String
    }

reason' :: Lens' Err (Maybe (Doc AnsiStyle))
reason' = lens reason (\s t -> s { reason = t })

footnotes' :: Lens' Err [Doc AnsiStyle]
footnotes' = lens footnotes (\s t -> s { footnotes = t })

expected' :: Lens' Err (Set String)
expected' = lens expected (\s t -> s { expected = t })

-- | Emit an ordinary error message.
--
-- >> show (emitPlainErr "unexpected input")
-- >> error: unexpected input
--
-- @since 0.1.0.0
emitPlainErr :: Doc AnsiStyle -> Err
emitPlainErr msg = Err (Just msg) [] mempty
