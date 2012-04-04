module Data.Lens.Partial.Common
  ( PartialLens(..)
  -- * Partial Lens construction
  , plens -- build a lens from a getter and setter
  , null
  , totalLens -- build a lens from a homomorphism
  -- * Functional API
  , runPLens
  , getPL
  , getorPL
  , getorAPL
  , nullPL
  , anyPL
  , allPL
  , trySetPL
  , setPL
  , modPL
  -- * Operator API
  , (^$)
  , (^.)
  , (^=)
  , (^%=)
  -- * Pseudo-imperatives
  , (^+=)
  , (^-=)
  , (^*=)
  , (^/=)
  -- * Stock lenses
  , justLens
  , leftLens
  , rightLens
  , headLens
  , tailLens
  , getorEmptyPL
  , sumPL
  , productPL
  ) where

import Prelude hiding ((.), id, null, any, all)
import Control.Applicative
import Control.Category
import Control.Category.Choice
import Control.Category.Product
import Control.Category.Codiagonal
import Data.Lens.Common (Lens(..))
import Control.Comonad.Trans.Store
import Data.Foldable (any, all)
import Data.Functor.Identity
import Data.Functor.Coproduct
import Data.Maybe
import Data.Monoid(Monoid)
import qualified Data.Monoid as M

newtype PartialLens a b = PLens (a -> Maybe (Store b a))

plens :: (a -> Maybe b) -> (a -> Maybe (b -> a)) -> PartialLens a b
plens get set = PLens $ \a -> do g <- get a
                                 s <- set a
                                 return (store s g)

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
getorPL :: PartialLens a b -> b -> a -> b
getorPL l b = fromMaybe b . getPL l

-- If the PartialLens is null, then return the given default value.
getorAPL :: Applicative f => PartialLens a b -> f b -> a -> f b
getorAPL l b = maybe b pure . getPL l

-- If the Partial is null.
nullPL :: PartialLens a b -> a -> Bool
nullPL l = isJust . getPL l

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

justLens :: PartialLens (Maybe a) a
justLens = PLens $ \ma -> do
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

getorEmptyPL :: (Monoid o) => PartialLens a b -> (b -> o) -> a -> o
getorEmptyPL l p = maybe M.mempty p . getPL l

-- returns 0 in case of null
sumPL :: (Num c) => PartialLens a b -> (b -> c) -> a -> c
sumPL l p = M.getSum . getorEmptyPL l (M.Sum . p)

-- returns 1 in case of null
productPL :: (Num c) => PartialLens a b -> (b -> c) -> a -> c
productPL l p = M.getProduct . getorEmptyPL l (M.Product . p)

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

instance Choice PartialLens where
  PLens f ||| PLens g =
    PLens $ either
      (fmap (\x -> store (Left . flip peek x) (pos x)) . f)
      (fmap (\y -> store (Right . flip peek y) (pos y)) . g)

instance Product PartialLens where
  PLens f *** PLens g =
    PLens $ \(a, c) ->
      do x <- f a
         y <- g c
         return $ store (\(b, d) -> (peek b x, peek d y)) (pos x, pos y)

instance Codiagonal PartialLens where
  codiagonal = id ||| id
