module Control.Category.Product where

class Product c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)
  