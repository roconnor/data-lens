module Control.Category.Choice where

infixr 2 |||
class Choice c where
  (|||) :: c x z -> c y z -> c (Either x y) z
