module Control.Category.Product where

import Prelude hiding (id)
import Control.Category

infixr 3 ***
infixr 3 &&&

class Category c => Tensor c where
  -- requires (fl *** fr) . (gl *** gr) === (fl . gl) *** (fr . gr)
  -- and id *** id === id
  (***) :: c w x -> c y z -> c (w, y) (x, z)
  first :: c w x -> c (w,z) (x,z)
  first = (*** id)
  second :: c y z -> c (w,y) (w,z)
  second = (id ***)

instance Tensor (->) where
  (***) f g (w, y) = (f w, g y)

class Tensor c => Product c where
  -- requires (f &&& g) . h === (f . h) &&& (g . h)
  -- and (fst &&& snd) === id
  -- and terminal = f 
  (&&&) :: c x y -> c x z -> c x (y, z)
  terminal :: c x ()

instance Product (->) where
  (&&&) f g x = (f x, g x)
  terminal _ = () 
