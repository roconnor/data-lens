module Control.Category.Product where

import Control.Arrow hiding ((***))
import Control.Applicative

infixr 3 ***
class Product c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)

instance Product (->) where
  (***) f g (w, y) =
    (f w, g y)

instance Applicative m => Product (Kleisli m) where
  Kleisli f *** Kleisli g =
    Kleisli $ \(w, y) -> liftA2 (,) (f w) (g y)
