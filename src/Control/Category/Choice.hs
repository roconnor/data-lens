module Control.Category.Choice where

import Control.Arrow hiding ((|||))

infixr 2 |||
class Choice c where
  (|||) :: c x z -> c y z -> c (Either x y) z

instance Choice (->) where
  (|||) = either

instance Choice (Kleisli m) where
  Kleisli f ||| Kleisli g = Kleisli $ either f g
