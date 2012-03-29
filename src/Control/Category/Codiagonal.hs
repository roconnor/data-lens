module Control.Category.Codiagonal where

import Control.Arrow
import Control.Applicative

class Codiagonal c where
  codiagonal :: c (Either a a) a

instance Codiagonal (->) where
  codiagonal = either id id

instance Applicative m => Codiagonal (Kleisli m) where
  codiagonal = Kleisli $ either pure pure
