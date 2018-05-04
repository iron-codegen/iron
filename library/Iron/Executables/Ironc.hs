module Iron.Executables.Ironc
  ( main
  ) where

import Iron.IR

import Iron.IR.Pretty (prettyDecl)
import System.IO (stdout)
import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty)

import qualified Text.PrettyPrint.ANSI.Leijen as PP

newtype D = D String
  deriving (Eq, Ord)

newtype B = B String
  deriving (Eq, Ord)

instance Pretty D where
  pretty (D d) = PP.bold . PP.green $ PP.text d

instance Pretty B where
  pretty (B b) = PP.bold . PP.yellow $ PP.text b

main :: IO ()
main =
  let
    a1, a2 :: ExprAnn
    a1 = ExprAnn False
    a2 = ExprAnn True

    d1, d2, d3 :: D
    d1 = D "d1"
    d2 = D "d2"
    d3 = D "d3"

    b1, b2, b3 :: B
    b1 = B "b1"
    b2 = B "b2"
    b3 = B "b3"

    decl2, decl3 :: Decl D B
    decl2 = ConstantDecl $
      mkLambdaExpr a1 [b1, b2, b3] $
        mkCallExpr a1 (mkDeclRefExpr a1 d1)
                      [ mkEvalExpr a1 (mkCallExpr a2 (mkBoundRefExpr a1 b1) [])
                                      (mkBoundRefExpr a1 b2)
                      , mkForceExpr a1 (mkBoundRefExpr a2 b3)
                      , mkDeferExpr a1 (mkDeclRefExpr a2 d1) ]
    decl3 = ConstantDecl $
      mkDeferExpr a1 $
        mkCallExpr a2 (mkDeclRefExpr a1 d2) []
  in
    PP.displayIO stdout . PP.renderPretty 1.0 80 $
      PP.vsep [ prettyDecl d2 decl2
              , prettyDecl d3 decl3 ]
