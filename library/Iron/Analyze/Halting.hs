-- | Does an expression halt?
module Iron.Analyze.Halting
  ( -- * Does an expression halt?
    Halts (..)
  , halts

    -- * Semigroups and monoids
  , All (..), _All, (.&&.)
  , Any (..), _Any, (.||.)
  ) where

import Control.Lens (Iso', auf, iso)
import Data.Semigroup (Semigroup)
import Iron.IR (Expr (..))
import Yaya.Control (Recursive, cata)



-- | Does an expression halt?
data Halts = No | Maybe | Yes
  deriving (Eq, Ord, Show)

-- | Does an expression halt?
halts :: (Recursive t (Expr d b)) => t -> Halts
halts = cata $ \case
  DeclRefExpr _  -> Yes
  BoundRefExpr _ -> Yes
  CallExpr f xs  -> f .&&. auf _All foldMap id xs
  LambdaExpr _ _ -> Yes
  EvalExpr a b   -> a .&&. b
  DeferExpr _    -> Yes
  ForceExpr x    -> Maybe .&&. x



-- | Do all of the expressions halt?
newtype All = All Halts

_All :: Iso' Halts All
_All = iso All (\(All a) -> a)

instance Semigroup All

instance Monoid All where
  mempty = All Yes
  mappend (All a) (All b) = All (a .&&. b)

(.&&.) :: Halts -> Halts -> Halts
(.&&.) Yes   Yes   = Yes
(.&&.) _     No    = No
(.&&.) No    _     = No
(.&&.) _     Maybe = Maybe
(.&&.) Maybe _     = Maybe

-- | Do any of the expressions halt?
newtype Any = Any Halts

_Any :: Iso' Halts Any
_Any = iso Any (\(Any a) -> a)

instance Semigroup Any

instance Monoid Any where
  mempty = Any No
  mappend (Any a) (Any b) = Any (a .||. b)

(.||.) :: Halts -> Halts -> Halts
(.||.) Yes   _     = Yes
(.||.) _     Yes   = Yes
(.||.) Maybe _     = Maybe
(.||.) _     Maybe = Maybe
(.||.) No    No    = No
