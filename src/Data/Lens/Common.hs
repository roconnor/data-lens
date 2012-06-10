module Data.Lens.Common
  ( Lens(..)
  -- * Lens construction
  , lens -- build a lens from a getter and setter
  , isoL -- build a lens from an isomorphism
  -- * Functional API
  , getL
  , fmodL
  , mergeL
  , unzipL
  -- * Operator API
  , (^$), (^$!)   -- getter -- :: Lens a b -> a -> b
  , (^.), (^!)    -- getter -- :: a -> Lens a b -> b
  -- * Mutators
  , set, setStrict, modify, modifyStrict
  , (^=), (^!=), (^%=), (^!%=), (^+=), (^!+=), (^-=), (^!-=), (^*=), (^!*=)
  , (^/=), (^!/=), (^&&=), (^||=), (^!&&=), (^!||=)
  -- * Stock lenses
  , fstL
  , sndL
  , mapL
  , intMapL
  , setL
  , intSetL
  ) where

import Control.Applicative
import Control.Comonad.Trans.Store
import Control.Category
import Control.Category.Product
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Semigroupoid
import Data.Isomorphism
import Data.Lens.Mutator
import Prelude hiding ((.), id)
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Set as Set
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

newtype Lens a b = Lens { runLens :: a -> Store b a }

instance Semigroupoid Lens where
  Lens f `o` Lens g = Lens $ \a -> case g a of
    StoreT wba b -> case f b of
      StoreT wcb c -> StoreT ((.) <$> wba <.> wcb) c

instance Category Lens where
  id = Lens $ StoreT (pure id)
  Lens f . Lens g = Lens $ \a -> case g a of
    StoreT wba b -> case f b of
      StoreT wcb c -> StoreT ((.) <$> wba <*> wcb) c

-- * Lens construction

-- | build a lens out of a getter and setter
lens :: (a -> b) -> (b -> a -> a) -> Lens a b
lens getter setter = Lens $ \a -> store (`setter` a) (getter a)

-- | build a lens out of an isomorphism
isoL :: Iso (->) a b -> Lens a b
isoL f = Lens (store (project f) . (embed f))

-- | Gets the getter function from a lens.
getL :: Lens a b -> a -> b
getL (Lens f) a = pos (f a)

infixr 0 ^$, ^$!
(^$), (^$!)  :: Lens a b -> a -> b
(^$) = getL
Lens f ^$! a = pos (f $! a)

infixl 9 ^., ^!
-- | functional getter, which acts like a field accessor
(^.), (^!) :: a -> Lens a b -> b
a ^. Lens f = pos (f a)
a ^! Lens f = pos (f $! a)

instance Mutator Lens where
  set (Lens f) b = peek b . f
  setStrict (Lens f) b = \a -> case f a of
    StoreT (Identity g) _ -> g $! b
  modify (Lens f) g = peeks g . f
  modifyStrict (Lens f) g = \a -> case f a of
    StoreT (Identity h) b -> h $! g b

fmodL :: Functor f => Lens a b -> (b -> f b) -> a -> f a
fmodL (Lens f) g = \a -> case f a of
  StoreT (Identity h) b -> h <$> g b
  
mergeL :: Lens a c -> Lens b c -> Lens (Either a b) c
Lens f `mergeL` Lens g = 
  Lens $ either (\a -> Left <$> f a) (\b -> Right <$> g b)

unzipL :: Lens a (b, c) -> (Lens a b, Lens a c)
unzipL f = (fstL . f, sndL . f)
  
-- * Stock lenses

fstL :: Lens (a,b) a
fstL = Lens $ \(a,b) -> store (\ a' -> (a', b)) a

sndL :: Lens (a,b) b
sndL = Lens $ \(a,b) -> store (\ b' -> (a, b')) b

mapL :: Ord k => k -> Lens (Map k v) (Maybe v)
mapL k = Lens $ \m -> store (\mv -> case mv of
    Nothing -> Map.delete k m
    Just v' -> Map.insert k v' m
  ) (Map.lookup k m)

intMapL :: Int -> Lens (IntMap v) (Maybe v)
intMapL k = Lens $ \m -> store (\mv -> case mv of
    Nothing -> IntMap.delete k m
    Just v' -> IntMap.insert k v' m
  ) (IntMap.lookup k m)

setL :: Ord k => k -> Lens (Set k) Bool
setL k = Lens $ \m -> store (\mv ->
    if mv then Set.insert k m else Set.delete k m
  ) (Set.member k m)

intSetL :: Int -> Lens IntSet Bool
intSetL k = Lens $ \m -> store (\mv ->
    if mv then IntSet.insert k m else IntSet.delete k m
  ) (IntSet.member k m)

instance Tensor Lens where
  Lens f *** Lens g =
    Lens $ \(a, c) ->
      let x = f a
          y = g c
      in store (\(b, d) -> (peek b x, peek d y)) (pos x, pos y)
