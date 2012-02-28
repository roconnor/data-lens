module Control.Category.Codiagonal where

import Data.Lens.Common
import Data.Lens.Partial.Common
import Prelude hiding (id)
import Control.Category(id)
import Control.Category.Choice

class Codiagonal c where
  codiagonal :: c (Either a a) a
  
instance Codiagonal Lens where
  codiagonal = id ||| id
  
instance Codiagonal PartialLens where
  codiagonal = id ||| id

