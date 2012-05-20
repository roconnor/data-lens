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
  MLens f . g = MLens $ g ^%%= f
   
-- totalLens is a homomorphism of categories; ie a functor.
totalLens :: Lens a b -> MultiLens a b
totalLens (Lens f) = MLens $ fromStore . f

-- totalLens is a homomorphism of categories; ie a functor.
partialLens :: PartialLens a b -> MultiLens a b
partialLens l = MLens $ coproduct (pure . runIdentity) fromStore . runPLens l

getML :: MultiLens a b -> a -> [b]
getML (MLens f) = poss . f

modML :: MultiLens a b -> (b -> b) -> a -> a
modML (MLens f) g = peekss g . f

infixr 4 ^%%=
-- | applicative modify
-- (id ^%%= h) = h
-- (f . g) ^%%= h) = (g ^%%= (f ^%%= h))
(^%%=) :: Applicative f => MultiLens a b -> (b -> f b) -> a -> f a
MLens f ^%%= g = go g . f 
  where
    {- this explicit passing of g is here to allow polymorphic recursion while remaining haskell 98 -} 
    go :: Applicative f => (b -> f b) -> (StaredStore b d) -> f d
    go k (StaredStore s) = coproduct (pure . runIdentity) (r k) s
    r :: Applicative f => (b -> f b) -> (StoreT b (StaredStore b) d) -> f d
    r k st = go k h <*> k v
      where 
        (h, v) = runStoreT st
