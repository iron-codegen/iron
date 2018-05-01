{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data structures for the intermediate representation.
module Iron.IR
  ( -- * Declarations
    Decl (..)

    -- * Expressions
  , AnnExpr
  , Expr (..)
  , ExprAnn (..)
  , mkDeclRefExpr
  , mkDeferExpr
  , mkForceExpr

    -- * Optics
  , HasEffectful (..)
  ) where

import Control.Comonad.Trans.Env (EnvT (..))
import Control.Lens (makeFields)
import Data.Vector (Vector)
import Yaya.Control (Embeddable, embed)
import Yaya.Data (Mu)

-- | A declaration, occurring at the top level.
--
-- The name of the declaration is stored elsewhere.
--
-- The type parameters are for the type of declaration names and bound names,
-- respectively.
data Decl d b
  -- | A declaration that names an expression that is immediately evaluated.
  = ConstantDecl (Mu (AnnExpr d b))

-- | An annotated expression.
type AnnExpr d b =
  EnvT ExprAnn (Expr d b)

-- | An expression, which evaluates to a result.
--
-- The type parameters are for the type of declaration names and bound names,
-- respectively.
data Expr d b e
  -- | Evaluate to the value of the given declaration.
  = DeclRefExpr d

  -- | Evaluate to the value of the given bound variable.
  | BoundRefExpr b

  -- | Evaluate to the return value of a function applied to a list of
  -- arguments.
  | CallExpr e (Vector e)

  -- | Evaluate to a lambda expression with some new bound variables.
  --
  -- The variables must not already be bound. In other words, shadowing is
  -- prohibited.
  | LambdaExpr (Vector b) e

  -- | Evaluate the first expression, then evaluate to the value of the second
  -- expression. Use this to prevent optimizing out the evaluation of an
  -- expression of which the result is unused.
  | EvalExpr e e

  -- | Do not evaluate the wrapped expression immediately, but leave this up to
  -- a 'ForceExpr'. Then, cache the value.
  | DeferExpr e

  -- | Evaluate a 'DeferExpr', unless already evaluated.
  | ForceExpr e

  deriving (Foldable, Functor, Traversable)

-- | An expression annotation.
data ExprAnn =
  ExprAnn
    { -- | State that the inner expression has side-effects. Such expressions
      -- will not be floated out, removed, duplicated, or reordered. Note that
      -- all parents should have this wrapper as well, otherwise it will act
      -- like 'System.IO.Unsafe.unsafePerformIO'.
      _exprAnnEffectful :: Bool }

mkDeclRefExpr :: Embeddable t (AnnExpr d b) => ExprAnn -> d -> t
mkDeclRefExpr a = embed . EnvT a . DeclRefExpr

mkDeferExpr :: Embeddable t (AnnExpr d b) => ExprAnn -> t -> t
mkDeferExpr a = embed . EnvT a . DeferExpr

mkForceExpr :: Embeddable t (AnnExpr d b) => ExprAnn -> t -> t
mkForceExpr a = embed . EnvT a . ForceExpr

$(makeFields ''ExprAnn)
