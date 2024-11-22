module TypeClass(Addable(..), Subtractable(..), Multipliable(..), Divisible(..)) where

-- 型クラスズ
class Addable a where
    (<+>) :: a -> a -> Maybe a

class Subtractable a where
    (<->) :: a -> a -> Maybe a

class Multipliable a where
    (<**>) :: a -> a -> Maybe a

class Divisible a where
    (</>) :: a -> a -> Maybe a
