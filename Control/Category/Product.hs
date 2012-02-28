module Control.Category.Product where

import Data.Lens.Common
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

infixr 3 ***
class Product c where
  (***) :: c w x -> c y z -> c (w, y) (x, z)
  
instance Product Lens where
  Lens f *** Lens g =
    Lens $ \(a, c) ->
      let x = f a
          y = g c
      in store (\(b, d) -> (peek b x, peek d y)) (pos x, pos y)

instance Product PartialLens where
  PLens f *** PLens g =
    PLens $ \(a, c) ->
      do x <- f a
         y <- g c
         return $ store (\(b, d) -> (peek b x, peek d y)) (pos x, pos y)

