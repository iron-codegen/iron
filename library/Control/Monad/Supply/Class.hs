-- | Infinite supplies of unique values.
module Control.Monad.Supply.Class
  ( MonadSupply
  , fresh
  ) where

class MonadSupply a m | m -> a where
  fresh :: m a
