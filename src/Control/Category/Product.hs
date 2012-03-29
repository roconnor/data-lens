module Control.Category.Product where

infixr 3 ***
class Product c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)

