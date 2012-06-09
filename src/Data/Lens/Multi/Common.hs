module Data.Lens.Multi.Common where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Applicative.Backwards
import Control.Category
import Data.Lens.Common (Lens(..), fstLens, sndLens)
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
  id = totalML id
  MLens f . g = MLens $ g `fmodML` f
   
-- totalLens is a homomorphism of categories; ie a functor.
totalML :: Lens a b -> MultiLens a b
totalML (Lens f) = MLens $ fromStore . f

-- totalLens is a homomorphism of categories; ie a functor.
partialML :: PartialLens a b -> MultiLens a b
partialML l = MLens $ coproduct (pure . runIdentity) fromStore . runPLens l

getML :: MultiLens a b -> a -> [b]
getML (MLens f) = poss . f

modML :: MultiLens a b -> (b -> b) -> a -> a
modML (MLens f) g = peekss g . f

-- | applicative modify
-- (id `fmodML` h) = h
-- ((f . g) `fmodML`  h) = g `amodL` (f `amodL` h))
fmodML :: Applicative f => MultiLens a b -> (b -> f b) -> a -> f a
MLens f `fmodML` g = eekss g . f 

frontPL :: MultiLens a b -> PartialLens a b
frontPL (MLens f) = pLens $
  coproduct left (right . uncurry store . (extract *** id) . runStoreT) . runStaredStore . f

reverseML :: MultiLens a b -> MultiLens a b
reverseML l = MLens (forwards . (l `fmodML` (Backwards . runMLens id)))

backPL :: MultiLens a b -> PartialLens a b
backPL = frontPL . reverseML

unzipML :: MultiLens a (b, c) -> (MultiLens a b, MultiLens a c)
unzipML l = (totalML fstLens . l, totalML sndLens . l)

isNullML :: MultiLens a b -> a -> Bool
isNullML l = null . getML l

-- Stock Multilenses

traversableML :: (Traversable f) => MultiLens (f a) a
traversableML = MLens $ traverse (runMLens id)

listML :: MultiLens [a] a
listML = traversableML

lookupByML :: (k -> Bool) -> MultiLens [(k,v)] v
lookupByML p = partialML keyPL . listML
  where
    keyPL = pLens f
    f (k,v) | p k = right (runLens sndLens (k,v))
            | otherwise = left (Identity (k,v))

lookupML :: (Eq k) => k -> MultiLens [(k,v)] v
lookupML k = lookupByML (k==)