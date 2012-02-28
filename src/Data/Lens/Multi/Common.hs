module Data.Lens.Multi.Common where

import Prelude hiding ((.), id, null)
import Control.Applicative
import Control.Category
import Data.Lens.Common (Lens(..))
import Data.Lens.Partial.Common (PartialLens(..), runPLens)
import Control.Comonad.Trans.Store
import Control.Comonad.StaredStore
import Data.Functor.Identity
import Data.Functor.Coproduct

newtype MultiLens a b = MLens {runMLens :: a -> StaredStore b a}

instance Category MultiLens where
  id = totalLens id
  MLens f . MLens g = MLens $ composeHelper f . g
   where
    {- this explicit passing of f is here to allow polymorphic recursion while remaining haskell 98 -} 
    composeHelper :: (b -> StaredStore c b) -> StaredStore b d -> StaredStore c d
    composeHelper k (StaredStore x) = coproduct (pure . runIdentity) h' x
     where
      h' y = composeHelper k v <*> k b
       where
        (v, b) = runStoreT y
   
-- totalLens is a homomorphism of categories; ie a functor.
totalLens :: Lens a b -> MultiLens a b
totalLens (Lens f) = MLens $ fromStore . f

-- totalLens is a homomorphism of categories; ie a functor.
partialLens :: PartialLens a b -> MultiLens a b
partialLens l = MLens $ coproduct (pure . runIdentity) fromStore . (runPLens l)

getML :: MultiLens a b -> a -> [b]
getML (MLens f) = poss . f

modML :: MultiLens a b -> (b -> b) -> a -> a
modML (MLens f) g = peekss g . f
