-- | Float expressions that have no free variables out to the top level.
module Iron.Optimize.Floating
  ( floatDecl
  , floatExpr
  ) where

import Control.Comonad.Trans.Env (lowerEnvT)
import Control.Lens ((^.))
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Writer.Class (MonadWriter)
import Data.Sequence (Seq)
import Iron.Analyze.FreeRefs (freeBoundRefs)
import Iron.IR (AnnExpr, Decl (..), ExprAnn (..), effectful, mkDeclRefExpr, mkDeferExpr, mkForceExpr)
import Yaya.Control (Embeddable, embed)
import Yaya.Data (Mu)
import Yaya.Extra (elgotZygoM)

import qualified Control.Comonad.Trans.Env as EnvT
import qualified Control.Monad.Supply.Class as Supply
import qualified Control.Monad.Writer.Class as Writer
import qualified Data.Set as Set

floatDecl :: ( Ord b
             , MonadSupply d m
             , MonadWriter (Seq (d, Decl d b)) m )
          => Decl d b -> m (Decl d b)
floatDecl (ConstantDecl e) =
  ConstantDecl <$> floatExpr e

floatExpr :: ( Ord b
             , MonadSupply d m
             , MonadWriter (Seq (d, Decl d b)) m )
          => Mu (AnnExpr d b)
          -> m (Mu (AnnExpr d b))
floatExpr = elgotZygoM (freeBoundRefs . lowerEnvT) $ \(bs, e) ->
              let pass = pure (embed e) in
              if | EnvT.ask e ^. effectful -> pass
                 | not (Set.null bs)       -> pass
                 | otherwise               -> forceFloatExpr (embed e)

forceFloatExpr :: ( Embeddable t (AnnExpr d b)
                  , MonadSupply d m
                  , MonadWriter (Seq (d, Decl d b)) m )
               => Mu (AnnExpr d b) -> m t
forceFloatExpr e = do
  floatName <- Supply.fresh
  Writer.tell [(floatName, ConstantDecl (mkDeferExpr deferAnn e))]
  pure $ mkForceExpr forceAnn (mkDeclRefExpr declRefAnn floatName)
  where
    declRefAnn, deferAnn, forceAnn :: ExprAnn
    declRefAnn = ExprAnn { _exprAnnEffectful = False }
    deferAnn   = ExprAnn { _exprAnnEffectful = False }
    forceAnn   = ExprAnn { _exprAnnEffectful = False }
