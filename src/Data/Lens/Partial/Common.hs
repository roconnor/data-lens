module Data.Lens.Partial.Common where

import Prelude hiding ((.), id, null)
import Control.Applicative
import Control.Category
import Data.Lens.Common (Lens(..))
import Control.Comonad.Trans.Store
import Data.Functor.Identity
import Data.Functor.Coproduct
import Data.Maybe

newtype PartialLens a b = PLens (a -> Maybe (Store b a))

-- A partial lens is a coalgebra for the Coproduct Identity (Store b) comonad.
runPLens :: PartialLens a b -> a -> (Coproduct Identity (Store b)) a
runPLens (PLens f) a = maybe (left (Identity a)) right (f a)

instance Category PartialLens where
  id = totalLens id
  PLens f . PLens g = PLens $ \a -> do
      (StoreT wba b) <- g a 
      (StoreT wcb c) <- f b
      return (StoreT ((.) <$> wba <*> wcb) c)

null :: PartialLens a b
null = PLens (const Nothing)

-- totalLens is a homomorphism of categories; ie a functor.
totalLens :: Lens a b -> PartialLens a b
totalLens (Lens f) = PLens (Just . f)

-- * Functional API

getPL :: PartialLens a b -> a -> Maybe b
getPL (PLens f) a = pos <$> f a

-- If the PartialLens is null, then return the given default value.
getorPL :: PartialLens a b -> a -> b -> b
getorPL l a b = fromMaybe b (getPL l a)

-- (Monadic) If the PartialLens is null, then return the given default value. Only the first effect is run.
getorMPL :: Monad m => PartialLens a b -> m a -> m b -> m b
getorMPL l a b = 
  do r <- a
     case getPL l r of
       Just c -> return c
       Nothing -> b

-- If the PartialLens is null, then return the target object, otherwise run the function on its projection.
withPL :: PartialLens a b -> (b -> a) -> a -> a
withPL l f = flip maybe f <*> getPL l

-- If the Partial is null.
isPL :: PartialLens a b -> a -> Bool
isPL l = isJust . getPL l

anyPL :: PartialLens a b -> (b -> Bool) -> a -> Bool
anyPL l p a =
  case getPL l a of
    Nothing -> False
    Just x -> p x

allPL :: PartialLens a b -> (b -> Bool) -> a -> Bool
allPL l p a =
  case getPL l a of
    Nothing -> True
    Just x -> p x

trySetPL :: PartialLens a b -> a -> Maybe (b -> a)
trySetPL (PLens f) a = flip peek <$> f a

-- If the PartialLens is null, then setPL returns the identity function.
setPL :: PartialLens a b -> b -> a -> a
setPL (PLens f) b a = maybe a (peek b) (f a)

-- If the PartialLens is null, then setPL returns the identity function.
modPL :: PartialLens a b -> (b -> b) -> a -> a
modPL (PLens f) g a = maybe a (peeks g) (f a)

-- * Operator API

infixr 0 ^$
(^$) :: PartialLens a b -> a -> Maybe b
(^$) = getPL

infixr 9 ^.
(^.) :: a -> PartialLens a b -> Maybe b
(^.) = flip getPL

infixr 4 ^=
(^=) :: PartialLens a b -> b -> a -> a
(^=) = setPL

infixr 4 ^%=
(^%=) :: PartialLens a b -> (b -> b) -> a -> a
(^%=) = modPL

-- * Pseudo-imperatives

infixr 4 ^+=, ^-=, ^*=
(^+=), (^-=), (^*=) :: Num b => PartialLens a b -> b -> a -> a
l ^+= n = l ^%= (+ n)
l ^-= n = l ^%= subtract n
l ^*= n = l ^%= (* n)

infixr 4 ^/=
(^/=) :: Fractional b => PartialLens a b -> b -> a -> a
l ^/= r = l ^%= (/ r)

-- * Stock partial lenses

maybeLens :: PartialLens (Maybe a) a
maybeLens = PLens $ \ma -> do
  a <- ma
  return (store Just a) 

leftLens :: PartialLens (Either a b) a
leftLens = PLens $ either (Just . store Left) (const Nothing)

rightLens :: PartialLens (Either a b) b
rightLens = PLens $ either (const Nothing) (Just . store Right)

headLens :: PartialLens [a] a
headLens = PLens f
 where
  f [] = Nothing
  f (h:t) = Just (store (:t) h)

tailLens :: PartialLens [a] [a]
tailLens = PLens f
 where
  f [] = Nothing
  f (h:t) = Just (store (h:) t)

{- Other Examples

nthLens :: Int -> PartialLens [a] a
nthLens n | n < 0  = null
          | n == 0 = headLens
          | otherwise = nthLens (n-1) . tailLens

-- setPL does not insert into a Map! it only modifies a value if the key already exists in the map
mapPLens :: Ord k => k -> PartialLens (Map.Map k v) v
mapPLens k = maybeLens . totalLens (mapLens k)

-- setPL does not insert into a IntMap! it only modifies a value if the key already exists in the map
intMapPLens :: Int -> PartialLens (IntMap v) v
intMapPLens k = maybeLens . totalLens (intMapLens k)
-}
