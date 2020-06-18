{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Module    :  Control.Carrier.Parser
-- Copyright   :  (c) Jacob Leach, 2020 - 2022
-- License     :  see LICENSE
--
-- Maintainer  :  leach.d.jake@gmail.com
-- Stability   :  stable
-- Portability :  non-portable
--
-- Parser effect carriers and evaluation.
--
-- @since 0.1.0.0

module Control.Carrier.Parser
  ( -- * Parser Carrier
    ParserC(..)
    -- ** Parser runners
  , runParser, runParserWithFile, runParserWith
  ) where

import           Control.Algebra
import           Control.Applicative
import           Control.Effect.Parser
import           Control.Effect.Throw
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Parser.Err
import           Data.Parser.Input
import           Data.Parser.Notice
import           Data.Set
import           Data.Source
import           Data.Text.Prettyprint.Doc

-- | Evaluates a parser effect.
--
-- @since 0.1.0.0
runParser
  :: forall m a b
  .  (Input -> a -> m b)
  -> (Err -> m b)
  -> Input
  -> ParserC m a
  -> m b
runParser leaf fail input (ParserC m) = m leaf fail input
{-# INLINE runParser #-}

-- | Evaluate a parser from a file path.
--
-- @since 0.1.0.0
runParserWithFile
  :: (MonadIO m, Has (Throw Notice) sig m)
  => FilePath
  -> ParserC m a
  -> m a
runParserWithFile path parser = do
  input <- liftIO (readFile path)
  runParserWith path (Input emptyPos mempty input) parser
{-# INLINE runParserWithFile #-}

-- | @since 0.1.0.0
runParserWith :: Has (Throw Notice) sig m => FilePath -> Input -> ParserC m a -> m a
runParserWith fp input = runParser (const pure) (throwError . errToNotice fp) input
{-# INLINE runParserWith #-}

-- | The carrier of a church-encoded parser effect.
--
-- @since 0.1.0.0
newtype ParserC m a = ParserC
  { runParserC
    :: forall r
    .  (Input -> a -> m r)
    -> (Err -> m r)
    -> Input
    -> m r
  }
  deriving Functor

-- | @since 0.1.0.0
instance Applicative (ParserC m) where
  pure a = ParserC $ \ k _ s -> k s a
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @since 0.1.0.0
instance Monad (ParserC m) where
  ParserC m >>= f = ParserC $ \ leaf fail ->
    m (\ inputs ->
      runParser leaf fail inputs . f) fail
  {-# INLINE (>>=) #-}

-- | @since 0.1.0.0
instance Alternative (ParserC m) where
  empty = ParserC (\_ fail input -> fail (Err input Nothing mempty))
  {-# INLINE empty #-}

  ParserC l <|> ParserC r = ParserC (\leaf fail input ->
    l leaf (\err1 ->
      r leaf (\err2 ->
        fail err1
          { errReason   = err1^.reason'   <|> err2^.reason'
          , errExpected = err2^.expected' <>  err1^.expected'
          }) input) input)
  {-# INLINE (<|>) #-}

-- | @since 0.1.0.0
instance Algebra sig m => Algebra (Parser :+: sig) (ParserC m) where
  alg hdl sig ctx = ParserC $ \ leaf fail inputs -> case sig of
    L (Satisfy p f) -> case inputs^.str' of
      c : cs | Just x <- p c -> runParser leaf fail (inputs { str = cs }) (hdl . (<$ ctx) . f $ x)
             | otherwise     -> fail (emitPlainErr (pretty "unexpected " <> pretty (show c)))
      []                     -> fail (emitPlainErr (pretty "unexpected end of input."))

    L (Unexpected msg parser) -> runParser leaf fail' inputs (hdl (parser <$ ctx))
      where fail' err = fail err { errExpected = singleton msg }

    R other -> thread (dst ~<~ hdl) other (pure ctx)
      >>= run . runParser (coerce leaf) (coerce fail) inputs
      where dst :: ParserC Identity (ParserC m a) -> m (ParserC Identity a)
            dst = run . runParser distParser (pure . cutfailk) inputs
  {-# INLINE alg #-}

distParser :: Applicative m => Input -> ParserC m a -> Identity (m (ParserC Identity a))
distParser inputs parser = return (runParser (const (pure . pure)) cutfailk inputs parser)
{-# INLINE distParser #-}

cutfailk :: Applicative m => Err -> m (ParserC Identity a)
cutfailk err = pure (ParserC (\ _ fail _ -> fail err))
{-# INLINE cutfailk #-}
