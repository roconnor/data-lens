module Data.Lens.Strict
  ( module Data.Lens.Common
  -- * State API
  , access         -- getter -- :: Monad m => Lens a b -> StateT a m b
  , (~=), (!=)     -- setter -- :: Monad m => Lens a b -> b -> StateT a m b
  , (%=), (!%=)    -- modify -- :: Monad m => Lens a b -> (b -> b) -> StateT a m b
  , (%%=), (!%%=)  -- modify -- :: Monad m => Lens a b -> (b -> (c, b)) -> StateT a m c
  , (+=), (!+=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (-=), (!-=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (*=), (!*=)    -- modify -- :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
  , (//=), (!/=)   -- modify -- :: (Monad m, Fractional b) => Lens a b -> b -> StateT a m b
  , (&&=), (!&&=)  -- modify -- :: Monad m => Lens a Bool -> Bool -> StateT a m Bool
  , (||=), (!||=)  -- modify -- :: Monad m => Lens a Bool -> Bool -> StateT a m Bool
  , focus          -- modify -- :: Monad m => Lens a b -> StateT m b c -> StateT m a c
  ) where

import Control.Arrow
import Control.Comonad.Trans.Store
import Control.Monad.Trans.State.Strict
import Control.Monad (liftM)
import Data.Functor.Identity
import Data.Lens.Common

-- * State actions

-- | get the value of a lens into state
access :: Monad m => Lens a b -> StateT a m b
access (Lens f) = gets (pos . f)
{-# INLINE access #-}

focus :: Monad m => Lens a b -> StateT b m c -> StateT a m c
focus (Lens f) (StateT g) = StateT $ \a -> case f a of
  StoreT (Identity h) b -> liftM (second h) (g b)

infixr 4 ~=, !=

-- | set a value using a lens into state
(~=), (!=) :: Monad m => Lens a b -> b -> StateT a m b
Lens f ~= b = StateT $ \a -> let c = peek b (f a) in 
    return (b, c)
Lens f != b = StateT $ \a -> case f a of
  StoreT (Identity h) _ -> let c = h $! b in
    return (b, c)

infixr 4 %=, !%=
    
-- | infix modification a value through a lens into state
(%=), (!%=) :: Monad m => Lens a b -> (b -> b) -> StateT a m b
Lens f %= g = StateT $ \a -> case f a of 
  StoreT (Identity h) b -> let b' = g b in
    return (b', h b')
Lens f !%= g = StateT $ \a -> case f a of
  StoreT (Identity h) b -> let b' = g b in
    b' `seq` return (b', h b')

infixr 4 %%=, !%%=

-- | infix modification of a value through a lens into state
-- with a supplemental response
(%%=), (!%%=) :: Monad m => Lens a b -> (b -> (c, b)) -> StateT a m c
Lens f %%= g = StateT $ \a -> case f a of
  StoreT (Identity h) b -> case g b of
    (c, b') -> return (c, h b')
Lens f !%%= g = StateT $ \a -> case f a of
  StoreT (Identity h) b -> case g b of
    (c, b') -> return (c, h $! b')

infixr 4 +=, !+=, -=, !-=, *=, !*=

(+=), (!+=), (-=), (!-=), (*=), (!*=) :: (Monad m, Num b) => Lens a b -> b -> StateT a m b
f += b = f %= (+ b)
f -= b = f %= subtract b
f *= b = f %= (* b)
f !+= b = f !%= (+ b)
f !-= b = f !%= subtract b
f !*= b = f !%= (* b)

infixr 4 //=, !/=

(//=), (!/=) :: (Monad m, Fractional b) => Lens a b -> b -> StateT a m b
f //= b = f %= (/ b)
f !/= b = f !%= (/ b)

infixr 4 &&=, !&&=, ||=, !||=

(&&=), (||=), (!&&=), (!||=) :: Monad m => Lens a Bool -> Bool -> StateT a m Bool
f &&= b = f %= (&& b)
f ||= b = f %= (|| b)
f !&&= b = f !%= (&& b)
f !||= b = f !%= (|| b)

