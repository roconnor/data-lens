module Data.Lens.Partial.Lazy where

import Control.Monad
import Control.Comonad.Trans.Store
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Lens.Partial.Common

maybeZero :: MonadPlus m => Maybe a -> m a
maybeZero Nothing = mzero
maybeZero (Just a) = return a

joinMaybe :: MonadPlus m => m (Maybe a) -> m a
joinMaybe = (maybeZero =<<)

-- * State actions

-- | get the value of a partial lens into state
access :: Monad m => PartialLens a b -> StateT a m (Maybe b)
access pl = getPL pl `liftM` get

-- | returns mzero in case of a null reference
accessPlus :: MonadPlus m => PartialLens a b -> StateT a m b
accessPlus = joinMaybe . access

-- | set a value using a partial lens into state
-- returns 'Nothing' in case of a null reference
(~=) :: Monad m => PartialLens a b -> b -> StateT a m (Maybe b)
(PLens f) ~= b = StateT $ \a -> return $ case f a of
  Nothing -> (Nothing, a)
  Just st -> (Just b, peek b st)

-- | infix modification a value through a partial lens into state
-- returns 'Nothing' in case of a null reference
(%=) :: Monad m => PartialLens a b -> (b -> b) -> StateT a m (Maybe b)
(PLens f) %= g = StateT $ \a -> return $ case f a of
  Nothing -> (Nothing, a)
  Just (StoreT (Identity h) b) -> let b' = g b in
    (Just b', h b')

-- | infix modification of a value through a partial lens into state
-- with a supplemental response.
-- returns 'Nothing' in case of a null reference
(%%=) :: Monad m => PartialLens a b -> (b -> (c, b)) -> StateT a m (Maybe c)
PLens f %%= g = StateT $ \a -> return $ case f a of
  Nothing -> (Nothing, a)
  Just (StoreT (Identity h) b) -> let (c,b') = g b in
    (Just c, h b')

infixr 4 +=, -=, *=

(+=), (-=), (*=) :: (Monad m, Num b) => PartialLens a b -> b -> StateT a m (Maybe b)
f += b = f %= (+ b)
f -= b = f %= subtract b
f *= b = f %= (* b)

infixr 4 //=

(//=) :: (Monad m, Fractional b) => PartialLens a b -> b -> StateT a m (Maybe b)
f //= b = f %= (/ b)

infixr 4 &&=, ||=

(&&=), (||=) :: Monad m => PartialLens a Bool -> Bool -> StateT a m (Maybe Bool)
f &&= b = f %= (&& b)
f ||= b = f %= (|| b)
