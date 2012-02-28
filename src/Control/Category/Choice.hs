module Control.Category.Choice where

import Data.Lens.Common
import Data.Lens.Partial.Common
import Control.Comonad.Trans.Store

infixr 2 |||
class Choice c where
  (|||) :: c x z -> c y z -> c (Either x y) z

instance Choice Lens where
  Lens f ||| Lens g =
    Lens $ either
      (\a -> let x = f a in store (Left . flip peek x) (pos x))
      (\b -> let y = g b in store (Right . flip peek y) (pos y))

instance Choice PartialLens where
  PLens f ||| PLens g =
    PLens $ either
      (\a -> fmap (\x -> store (Left . flip peek x) (pos x)) (f a))
      (\b -> fmap (\y -> store (Right . flip peek y) (pos y)) (g b))

