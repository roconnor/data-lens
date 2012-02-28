module Control.Category.Choice where

class Choice c where
  (|||) :: c x z -> c y z -> c (Either x y) z
  