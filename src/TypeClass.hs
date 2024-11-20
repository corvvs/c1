module TypeClass(Addable(..), Subtractable(..), Multipliable(..), Divisible(..)) where

-- 型クラスズ
class Addable a where
    add :: a -> a -> Maybe a

class Subtractable a where
    sub :: a -> a -> Maybe a

class Multipliable a where
    mul :: a -> a -> Maybe a

class Divisible a where
    div :: a -> a -> Maybe a
