module Data.Lens.Partial.Common where

import Prelude hiding ((.), id, null, any, all)
import Control.Applicative
import Control.Category
import Control.Category.Product
import Data.Lens.Common (Lens(..), fstLens, sndLens)
import Control.Comonad.Trans.Store
import Data.Foldable (any, all)
import Data.Functor.Identity
import Data.Functor.Coproduct
import Data.Maybe
import Data.Monoid

newtype PartialLens a b = PLens (a -> Maybe (Store b a))

pLens :: (a -> Coproduct Identity (Store b) a) -> PartialLens a b
pLens f = PLens $ coproduct (const Nothing) Just . f

-- A partial lens is a coalgebra for the Coproduct Identity (Store b) comonad.
runPLens :: PartialLens a b -> a -> (Coproduct Identity (Store b)) a
runPLens (PLens f) a = maybe (left (Identity a)) right (f a)

instance Category PartialLens where
  id = totalPL id
  PLens f . PLens g = PLens $ \a -> do
      (StoreT wba b) <- g a 
      (StoreT wcb c) <- f b
      return (StoreT ((.) <$> wba <*> wcb) c)

nullPL :: PartialLens a b
nullPL = PLens (const Nothing)

-- totalPL is a homomorphism of categories; ie a functor.
totalPL :: Lens a b -> PartialLens a b
totalPL (Lens f) = PLens (Just . f)

-- * Functional API

getPL :: PartialLens a b -> a -> Maybe b
getPL (PLens f) a = pos <$> f a

-- If the PartialLens is null, then return the given default value.
getorPL :: PartialLens a b -> b -> a -> b
getorPL l b = fromMaybe b . getPL l

-- If the PartialLens is null, then return the given default value.
getorAPL :: Applicative f => PartialLens a b -> f b -> a -> f b
getorAPL l b = maybe b pure . getPL l

mergePL :: PartialLens a c -> PartialLens b c -> PartialLens (Either a b) c
(PLens f) `mergePL` (PLens g) =
  PLens $ either (\a -> (fmap Left) <$> f a) (\b -> (fmap Right) <$> g b)

unzipPL :: PartialLens a (b, c) -> (PartialLens a b, PartialLens a c)
unzipPL f = (totalPL fstLens . f, totalPL sndLens . f)
  
-- If the Partial is null.
isNullPL :: PartialLens a b -> a -> Bool
isNullPL l = isNothing . getPL l

getorEmptyPL :: (Monoid o) => PartialLens a b -> (b -> o) -> a -> o
getorEmptyPL l p = maybe mempty p . getPL l

emptyPL :: Monoid b => PartialLens a b -> a -> b
emptyPL = flip getorEmptyPL id

-- returns 0 in case of null
sumPL :: (Num c) => PartialLens a b -> (b -> c) -> a -> c
sumPL l p = getSum . getorEmptyPL l (Sum . p)

-- returns 1 in case of null
productPL :: (Num c) => PartialLens a b -> (b -> c) -> a -> c
productPL l p = getProduct . getorEmptyPL l (Product . p)

anyPL :: PartialLens a b -> (b -> Bool) -> a -> Bool
anyPL l p =
  any p . getPL l

allPL :: PartialLens a b -> (b -> Bool) -> a -> Bool
allPL l p =
  all p . getPL l

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

infixl 9 ^.
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

justPL :: PartialLens (Maybe a) a
justPL = PLens $ \ma -> do
  a <- ma
  return (store Just a) 

leftPL :: PartialLens (Either a b) a
leftPL = PLens $ either (Just . store Left) (const Nothing)

rightPL :: PartialLens (Either a b) b
rightPL = PLens $ either (const Nothing) (Just . store Right)

headPL :: PartialLens [a] a
headPL = PLens f
 where
  f [] = Nothing
  f (h:t) = Just (store (:t) h)

tailPL :: PartialLens [a] [a]
tailPL = PLens f
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
mapPLens k = justLens . totalPL (mapLens k)

-- setPL does not insert into a IntMap! it only modifies a value if the key already exists in the map
intMapPLens :: Int -> PartialLens (IntMap v) v
intMapPLens k = justLens . totalPL (intMapLens k)
-}

instance Tensor PartialLens where
  PLens f *** PLens g =
    PLens $ \(a, c) ->
      do x <- f a
         y <- g c
         return $ store (\(b, d) -> (peek b x, peek d y)) (pos x, pos y)
