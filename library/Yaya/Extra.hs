module Yaya.Extra
  ( ElgotAlgebraM
  , elgotZygoM
  ) where

import Yaya (Algebra)
import Yaya.Control (Recursive, distZygo, elgotCataM)

type ElgotAlgebraM m w f a = w (f a) -> m a

elgotZygoM :: (Monad m, Recursive t f, Traversable f)
           => Algebra f b
           -> ElgotAlgebraM m ((,) b) f a
           -> t
           -> m a
elgotZygoM φ' φ = elgotCataM (distZygo φ') φ
