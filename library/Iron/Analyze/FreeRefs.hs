module Iron.Analyze.FreeRefs
  ( freeBoundRefs
  ) where

import Data.Foldable (fold)
import Data.Semigroup ((<>))
import Data.Set (Set, (\\))
import Iron.IR (Expr (..))

import qualified Data.Set as Set

-- | Return the free variables in the expression.
freeBoundRefs :: Ord b => Expr d b (Set b) -> Set b
freeBoundRefs (DeclRefExpr _  ) = Set.empty
freeBoundRefs (BoundRefExpr b ) = Set.singleton b
freeBoundRefs (CallExpr f xs  ) = f <> fold xs
freeBoundRefs (LambdaExpr vs x) = x \\ foldr Set.insert Set.empty vs
freeBoundRefs (EvalExpr x y   ) = x <> y
freeBoundRefs (DeferExpr x    ) = x
freeBoundRefs (ForceExpr x    ) = x
