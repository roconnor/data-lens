module Data.Lens.Multi.Common where

import Prelude hiding ((.), id, null)
import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Data.Lens.Common (Lens(..), sndLens)
import Data.Lens.Partial.Common (PartialLens, pLens, runPLens)
import Control.Comonad
import Control.Comonad.Trans.Store
import Control.Comonad.StaredStore
import Data.Functor.Identity
import Data.Functor.Coproduct
import Data.Traversable
import Control.Arrow ((***))

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
MLens f ^%%= g = eekss g . f 

backPL :: MultiLens a b -> PartialLens a b
backPL (MLens f) = pLens $
  coproduct left (right . uncurry store . (extract *** id) . runStoreT) . runStaredStore . f

reverseML :: MultiLens a b -> MultiLens a b
reverseML l = MLens (forwards . (l ^%%= (Backwards . runMLens id)))

frontPL :: MultiLens a b -> PartialLens a b
frontPL = backPL . reverseML

-- Stock Multilenses

traversableLens :: (Traversable f) => MultiLens (f a) a
traversableLens = MLens $ traverse (runMLens id)

listLens :: MultiLens [a] a
listLens = traversableLens

lookupByL :: (k -> Bool) -> MultiLens [(k,v)] v
lookupByL p = partialLens keyPL . listLens
  where
    keyPL = pLens f
    f (k,v) | p k = right (runLens sndLens (k,v))
            | otherwise = left (Identity (k,v))

lookupL :: (Eq k) => k -> MultiLens [(k,v)] v
lookupL k = lookupByL (k==)