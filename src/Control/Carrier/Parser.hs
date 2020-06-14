{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |
--
-- @since 0.1.0.0

module Control.Carrier.Parser where

import           Control.Algebra
import           Control.Applicative          as Alternative
import           Control.Carrier.State.Strict
import           Control.Effect.Parser
import           Control.Lens
import           Control.Monad
import           Control.Monad.Iteratee
import           Data.Parser.Err
import Data.Source
import           Data.Parser.Input
import Data.Functor
import           Data.Set
import           Data.Text.Prettyprint.Doc


runParser
  :: (Input -> a -> m r)
  -> (Err -> m r)
  -> (Err -> m r)
  -> Input
  -> ParserC m a
  -> m r
runParser leaf nil fail input (ParserC run) = run leaf nil fail input
{-# INLINE runParser #-}

-- | @since 0.1.0.0
newtype ParserC m a = ParserC
  (forall r
   . (Input -> a -> m r)       -- success
  -> (Err -> m r) -- empty
  -> (Err -> m r) -- cut
  -> Input
  -> m r)
  deriving (Functor)

instance Applicative (ParserC m) where
  pure x = ParserC (\leaf _ _ inputs -> leaf inputs x)
  {-# INLINE pure #-}

  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (ParserC m) where
  ParserC m >>= f = ParserC (\ leaf nil fail -> m (\ input -> runParser leaf nil fail input . f) nil fail)

instance Semigroup a => Semigroup (ParserC m a) where
  (<>) = liftA2 (<>)
  {-# inlinable (<>) #-}

instance (Semigroup a, Monoid a) => Monoid (ParserC m a) where
  mappend = (<>)
  {-# inlinable mappend #-}

  mempty = pure mempty
  {-# inlinable mempty #-}

instance Algebra sig m => Algebra (Parser :+: sig) (ParserC m) where
  alg hdl sig ctx = case sig of
    L (Accept p f) -> ParserC $ \leaf nil _ inputs -> case inputs^.str' of
      c : cs | Just a <- p c -> do
                 inputs' <- evalState inputs advance
                 leaf inputs' (f a <$ ctx)

             | otherwise     -> nil (emitPlainErr (pretty "unexpected" <> pretty (show c)))
      []                     -> nil (emitPlainErr (pretty "unexpected end of input"))

    L Delta -> ParserC $ \leaf _ _ inputs -> leaf inputs (inputs^.delta' <$ ctx)
