module Iron.IR.Pretty
  ( prettyDecl
  , prettyExpr
  , prettyWithExprAnn
  ) where

import Control.Category ((>>>))
import Control.Comonad.Trans.Env (EnvT (..))
import Control.Lens ((^.))
import Data.Foldable (toList)
import Iron.IR (AnnExpr, Decl (..), Expr (..), ExprAnn, effectful)
import Text.PrettyPrint.ANSI.Leijen (Doc, Pretty, (<+>), pretty)
import Yaya.Control (Recursive, cata)

import qualified Text.PrettyPrint.ANSI.Leijen as PP



prettyDecl :: (Pretty d, Pretty b) => d -> Decl d b -> Doc
prettyDecl name (ConstantDecl value) =
  hang [ keyword "const" <+> pretty name <+> punctuation "="
       , prettyExpr value ]

prettyExpr :: ( Recursive t (AnnExpr d b)
              , Pretty d, Pretty b ) => t -> Doc
prettyExpr = cata . underAnnExpr $ \case
  DeclRefExpr d   -> PP.pretty d
  BoundRefExpr b  -> PP.pretty b
  CallExpr f xs   -> keyword "call" <+> f <+> tupled xs
  LambdaExpr vs x -> hang [keyword "lambda" <+> tupled (fmap pretty vs) <+> punctuation "=>", x]
  EvalExpr x y    -> hang [ keyword "eval"
                          , hang [keyword "discard", x]
                          , hang [keyword "yield", y] ]
  DeferExpr x     -> hang [keyword "defer", x]
  ForceExpr x     -> hang [keyword "force", x]
  where
    underAnnExpr :: (Expr d b Doc -> Doc) -> AnnExpr d b Doc -> Doc
    underAnnExpr f (EnvT a e) = prettyWithExprAnn a (f e)

prettyWithExprAnn :: ExprAnn -> Doc -> Doc
prettyWithExprAnn a e
  | a ^. effectful = hang [keyword "effect", e]
  | otherwise      = e



hang :: [Doc] -> Doc
hang ds = PP.hang 2 (PP.vsep ds)

keyword :: String -> Doc
keyword = PP.bold . PP.blue . PP.text

punctuation :: String -> Doc
punctuation = PP.bold . PP.dullwhite . PP.text

tupled :: Foldable f => f Doc -> Doc
tupled = toList >>> \case
  [] -> punctuation "(" <+> punctuation ")"
  [x] -> punctuation "(" <+> x <+> punctuation ")"
  xs -> PP.encloseSep (punctuation "( ")
                      (punctuation " )")
                      (punctuation ", ") xs
