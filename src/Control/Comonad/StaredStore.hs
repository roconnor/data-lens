module Control.Comonad.StaredStore where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Store
import Data.Functor.Coproduct
import Data.Functor.Identity
import Data.Functor.Constant

newtype StaredStore s a = StaredStore {runStaredStore :: Coproduct Identity (StoreT s (StaredStore s)) a}

instance Functor (StaredStore s) where
  fmap f (StaredStore x) = StaredStore (fmap f x)
  
instance Extend (StaredStore s) where
  duplicate (StaredStore x) = StaredStore (fmap StaredStore (duplicate x))
  extend f (StaredStore x) = StaredStore (extend (f . StaredStore) x)
  
instance Comonad (StaredStore s) where
  extract (StaredStore x) = extract x

instance Applicative (StaredStore s) where
  pure = StaredStore . left . pure
  (StaredStore f) <*> x = coproduct l r f
   where
    l (Identity g) = fmap g x
    r g = StaredStore (right (StoreT (flip <$> h <*> x) s))
      where
        (h, s) = runStoreT g

fromStore :: Store b a -> StaredStore b a
fromStore st = StaredStore (right (StoreT (pure g) v))
 where
  (g,v) = runStore st
  
poss :: StaredStore b a -> [b]
poss = getConstant . eekss (Constant . (:[]))

seekss :: (b -> b) -> StaredStore b a -> StaredStore b a
seekss f = coproduct (pure . runIdentity) h . runStaredStore
 where
  h s = StaredStore (right (StoreT (seekss f g) (f v)))
   where
    (g, v) = runStoreT s

peekss :: (b -> b) -> StaredStore b a -> a
peekss f = extract . seekss f

-- eeks :: Functor f => (b -> f b) -> Store b a -> f a
eekss :: Applicative f => (b -> f b) -> (StaredStore b a) -> f a
eekss f (StaredStore s) = coproduct (pure . runIdentity) h s
  where
    h st = f v <**> eekss f g
      where 
        (g, v) = runStoreT st
