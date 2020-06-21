
-- | Module    :  Control.Effect.Parser
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Syntax of the parser effect.
--
-- @since 0.1.0.0

module Control.Effect.Parser
  ( -- * Parser Combinators
    peek, char, between, option, skipSpace, passes
    -- ** Parser Operators
  , (<!>)
    -- * Re-exports
  , module Control.Effect.Parser.Internal
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser.Internal
import           Control.Monad
import           Data.Char
import           Data.Text

-- | Peek at the current element in the Parser's input.
--
-- @since 0.1.0.0
peek :: Has Parser sig m => m Char
peek = satisfy Just pure
{-# INLINE CONLIKE peek #-}

-- | Tests if a character matches the predicate @p@
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
char :: Has Parser sig m => Char -> m Char
char = passes . (==)
{-# INLINE char #-}

-- | Returns a character if it satisfies the given predicate.
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
passes :: Has Parser sig m => (Char -> Bool) -> m Char
passes p = satisfy (\c -> if p c then Just c else Nothing) pure
{-# INLINE passes #-}

-- | Optionally evaluates a @parser@ returning "Nothing" if it fails
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
option :: (Alternative m, Has Parser sig m) => m k -> m (Maybe k)
option parser = (parser >>= return . Just) <|> return Nothing
{-# INLINEABLE option #-}

-- | Skips whitespace characters.
--
-- \(\mathcal{O}(n)\), where n is the number of whitespace characters.
--
-- @since 0.1.0.0
skipSpace :: (Alternative m, Has Parser sig m) => m ()
skipSpace = void (many (passes isSpace))
{-# INLINE skipSpace #-}

-- | Takes the result of a parse inbetween two heterogeneous combinators.
--
-- @since 0.1.0.0
between :: Has Parser sig m => m k -> m k -> m k -> m k
between o b c = o *> b <* c
{-# INLINEABLE between #-}

-- | Infix notation for 'unexpected'.
--
-- \(\mathcal{O}(1)\).
--
-- @since 0.1.0.0
(<!>) :: Has Parser sig m => m k -> Text -> m k
(<!>) = flip unexpected
{-# INLINE (<!>) #-}
