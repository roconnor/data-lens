module Control.Comonad.StaredStore where

import Control.Applicative
import Control.Comonad
import Control.Comonad.Trans.Store
import Data.Functor.Coproduct
import Data.Functor.Identity

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
  f <*> (StaredStore x) = coproduct l r x
   where
    l (Identity y) = fmap ($ y) f
    r y = StaredStore (right (StoreT ((.) <$> f <*> v) s))
      where
        (v, s) = runStoreT y

fromStore :: Store b a -> StaredStore b a
fromStore st = StaredStore (right (StoreT (pure g) v))
 where
  (g,v) = runStore st
  
poss :: StaredStore b a -> [b]
poss x = go x []
 where
  go :: StaredStore b a -> [b] -> [b]
  go = coproduct (const id) h . runStaredStore
  h s = go g . (v:)
   where
    (g, v) = runStoreT s

seekss :: (b -> b) -> StaredStore b a -> StaredStore b a
seekss f = coproduct (pure . runIdentity) h . runStaredStore
 where
  h s = StaredStore (right (StoreT (seekss f g) (f v)))
   where
    (g, v) = runStoreT s

peekss :: (b -> b) -> StaredStore b a -> a
peekss f = extract . seekss f
