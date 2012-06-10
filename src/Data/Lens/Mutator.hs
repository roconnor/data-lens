module Data.Lens.Mutator where

class Mutator l where
  modify, modifyStrict, (^%=), (^!%=) :: l a b -> (b -> b) -> a -> a
  (^%=) = modify
  (^!%=) = modifyStrict
  set, setStrict, (^=), (^!=) :: l a b -> b -> (a -> a)
  set l v = modify l (const v)
  setStrict l v = modifyStrict l (const v)
  (^=) = set
  (^!=) = setStrict

infixr 4 ^+=, ^!+=, ^-=, ^!-=, ^*=, ^!*=
(^+=), (^!+=), (^-=), (^!-=), (^*=), (^!*=) :: (Mutator l, Num b) => l a b -> b -> a -> a
l ^+= n = l ^%= (+ n)
l ^-= n = l ^%= subtract n
l ^*= n = l ^%= (* n)
l ^!+= n = l ^!%= (+ n)
l ^!-= n = l ^!%= subtract n
l ^!*= n = l ^!%= (* n)

infixr 4 ^/=, ^!/=
(^/=), (^!/=) :: (Mutator l, Fractional b) => l a b -> b -> a -> a
l ^/= r = l ^%= (/ r)
l ^!/= r = l ^!%= (/ r)

infixr 4 ^&&=, ^!&&=, ^||=, ^!||=
(^&&=), (^||=), (^!&&=), (^!||=) :: Mutator l => l a Bool -> Bool -> a -> a
l ^&&= b = l ^%= (&& b)
l ^||= b = l ^%= (|| b)
l ^!&&= b = l ^!%= (&& b)
l ^!||= b = l ^!%= (|| b)
