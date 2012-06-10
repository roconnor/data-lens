module Data.Lens.Lazy.Void
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

import Data.Lens.Common
import Control.Monad (void)
import Control.Monad.Trans.State
import Data.Lens.Lazy (access, (%%=), (!%%=), focus)
import qualified Data.Lens.Lazy as L

-- * State actions

infixr 4 ~=, !=

-- | set a value using a lens into state
(~=), (!=) :: (Functor m, Monad m) => Lens a b -> b -> StateT a m ()
l ~= b = void (l L.~= b)
l != b = void (l L.!= b)

infixr 4 %=, !%=
    
-- | infix modification a value through a lens into state
(%=), (!%=) :: (Functor m, Monad m) => Lens a b -> (b -> b) -> StateT a m ()
l %= f = void (l L.%= f)
l !%= f = void (l L.!%= f)

infixr 4 +=, !+=, -=, !-=, *=, !*=

(+=), (!+=), (-=), (!-=), (*=), (!*=) :: (Functor m, Monad m, Num b) => Lens a b -> b -> StateT a m ()
f += b = f %= (+ b)
f -= b = f %= subtract b
f *= b = f %= (* b)
f !+= b = f !%= (+ b)
f !-= b = f !%= subtract b
f !*= b = f !%= (* b)

infixr 4 //=, !/=

(//=), (!/=) :: (Functor m, Monad m, Fractional b) => Lens a b -> b -> StateT a m ()
f //= b = f %= (/ b)
f !/= b = f !%= (/ b)

infixr 4 &&=, !&&=, ||=, !||=

(&&=), (||=), (!&&=), (!||=) :: (Functor m, Monad m) => Lens a Bool -> Bool -> StateT a m ()
f &&= b = f %= (&& b)
f ||= b = f %= (|| b)
f !&&= b = f !%= (&& b)
f !||= b = f !%= (|| b)