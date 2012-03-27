module Data.Lens.Common
  ( Lens(..)
  -- * Lens construction
  , lens -- build a lens from a getter and setter
  , iso  -- build a lens from an isomorphism
  -- * Functional API
  , getL
  , setL
  , modL
  -- * Operator API
  , (^$),  (^$!)   -- getter -- :: Lens a b -> a -> b
  , (^.),  (^!)    -- getter -- :: a -> Lens a b -> b
  , (^=),  (^!=)   -- setter -- :: Lens a b -> b -> (a -> a)
  , (^%=), (^!%=)  -- modify -- :: Lens a b -> (b -> b) -> (a -> a)
  , (^%%=)         -- modify -- :: Functor f => Lens a b -> (b -> f b) -> a -> f a
  -- * Pseudo-imperatives
  , (^+=), (^!+=) -- addition
  , (^-=), (^!-=) -- subtraction
  , (^*=), (^!*=) -- multiplication
  , (^/=), (^!/=) -- division
  -- * Stock lenses
  , fstLens
  , sndLens
  , mapLens
  , intMapLens
  , setLens
  , intSetLens
  ) where

import Control.Applicative
import Control.Comonad.Trans.Store
import Control.Category
import Data.Functor.Identity
import Data.Functor.Apply
import Data.Semigroupoid
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
lens get set = Lens $ \a -> store (`set` a) (get a)

-- | build a lens out of an isomorphism
iso :: (a -> b) -> (b -> a) -> Lens a b
iso f g = Lens (store g . f)

-- | Gets the getter function from a lens.
getL :: Lens a b -> a -> b
getL (Lens f) a = pos (f a)

infixr 0 ^$, ^$!
(^$), (^$!)  :: Lens a b -> a -> b
(^$) = getL
Lens f ^$! a = pos (f $! a)

infixr 9 ^., ^!
-- | functional getter, which acts like a field accessor
(^.), (^!) :: a -> Lens a b -> b
a ^. Lens f = pos (f a)
a ^! Lens f = pos (f $! a)

-- | Gets the setter function from a lens.
setL :: Lens a b -> b -> a -> a
setL (Lens f) b = peek b . f

infixr 4 ^=, ^!=
(^=), (^!=) :: Lens a b -> b -> a -> a
(^=) = setL
Lens f ^!= b = \a -> case f a of
  StoreT (Identity g) _ -> g $! b

-- | Gets the modifier function from a lens.
modL :: Lens a b -> (b -> b) -> a -> a
modL (Lens f) g = peeks g . f

infixr 4 ^%=, ^!%=
-- | functional modify
(^%=), (^!%=) :: Lens a b -> (b -> b) -> a -> a
(^%=) = modL
Lens f ^!%= g = \a -> case f a of
  StoreT (Identity h) b -> h $! g b

infixr 4 ^%%=
-- | functorial modify
(^%%=) :: Functor f => Lens a b -> (b -> f b) -> a -> f a
Lens f ^%%= g = \a -> case f a of
  StoreT (Identity h) b -> h <$> g b

infixr 4 ^+=, ^!+=, ^-=, ^!-=, ^*=, ^!*=
(^+=), (^!+=), (^-=), (^!-=), (^*=), (^!*=) :: Num b => Lens a b -> b -> a -> a
l ^+= n = l ^%= (+ n)
l ^-= n = l ^%= subtract n
l ^*= n = l ^%= (* n)
l ^!+= n = l ^!%= (+ n)
l ^!-= n = l ^!%= subtract n
l ^!*= n = l ^!%= (* n)

infixr 4 ^/=, ^!/=
(^/=), (^!/=) :: Fractional b => Lens a b -> b -> a -> a
l ^/= r = l ^%= (/ r)
l ^!/= r = l ^!%= (/ r)

-- * Stock lenses

fstLens :: Lens (a,b) a
fstLens = Lens $ \(a,b) -> store (\ a' -> (a', b)) a

sndLens :: Lens (a,b) b
sndLens = Lens $ \(a,b) -> store (\ b' -> (a, b')) b

mapLens :: Ord k => k -> Lens (Map k v) (Maybe v)
mapLens k = Lens $ \m -> store (\mv -> case mv of
    Nothing -> Map.delete k m
    Just v' -> Map.insert k v' m
  ) (Map.lookup k m)

intMapLens :: Int -> Lens (IntMap v) (Maybe v)
intMapLens k = Lens $ \m -> store (\mv -> case mv of
    Nothing -> IntMap.delete k m
    Just v' -> IntMap.insert k v' m
  ) (IntMap.lookup k m)

setLens :: Ord k => k -> Lens (Set k) Bool
setLens k = Lens $ \m -> store (\mv ->
    if mv then Set.insert k m else Set.delete k m
  ) (Set.member k m)

intSetLens :: Int -> Lens IntSet Bool
intSetLens k = Lens $ \m -> store (\mv ->
    if mv then IntSet.insert k m else IntSet.delete k m
  ) (IntSet.member k m)
