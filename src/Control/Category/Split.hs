module Control.Category.Split where

infixr 3 ***
class Split c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)

