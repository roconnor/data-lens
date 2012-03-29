module Control.Category.Split where

import Control.Arrow
import Control.Applicative

infixr 3 ***
class Split c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)

instance Split (->) where
  (***) f g (w, y) =
    (f w, g y)

instance Applicative m => Split (Kleisli m) where
  Kleisli f *** Kleisli g =
    Kleisli $ \(w, y) -> liftA2 (,) (f w) (g y)
