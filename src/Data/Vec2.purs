module Data.Vec2 where

import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D2, d0, d1)
import Data.Vec (Vec, (!!))

-- I like using these over tuples when possible because I can use operators like + and - on them.
-- | Pair of 2 values of the same type.
type Vec2 = Vec D2

toTuple :: forall a. Vec2 a -> Tuple a a
toTuple vec = Tuple (vec !! d0) (vec !! d1) 