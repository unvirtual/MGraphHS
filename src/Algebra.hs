{-# LANGUAGE TypeFamilies #-}

module Algebra where

import Data.Ratio

{----------------------------------------------------------------------
 -
 - Additive Group
 -
 ----------------------------------------------------------------------}

infixl 6 .+.
infixl 6 .-.

class AddGroup v where
    (.+.) :: v -> v -> v
    neg   :: v -> v
    (.-.) :: v -> v -> v
    zero  :: v
    sum   :: [v] -> v

    sum = foldr (.+.) zero
    v1 .-. v2 = v1 .+. (neg v2)

instance (Integral a) => AddGroup (Ratio a) where
    zero = 0
    (.+.) = (+)
    neg = negate

instance AddGroup Double where
    zero = 0
    (.+.) = (+)
    neg = negate

instance AddGroup Float where
    zero = 0
    (.+.) = (+)
    neg = negate


{----------------------------------------------------------------------
 -
 - Module
 -
 ----------------------------------------------------------------------}

infixr 7 *>
infixl 7 <*
infixr 7 </

class (AddGroup v) => Module v where
    type Scalar v :: *
    (*>)  :: Scalar v -> v -> v

(<*) :: (Module v, s ~ Scalar v) => v -> s -> v
(<*) = flip (*>)

(</) :: (Module v, s ~ Scalar v, Fractional s) => v -> s -> v
v </ s = (1/s) *> v

instance (Integral a) => Module (Ratio a) where
    type Scalar (Ratio a) = Ratio a
    (*>)  = (*)

instance Module Double where
    type Scalar Double = Double
    (*>)  = (*)

instance Module Float where
    type Scalar Float = Float
    (*>)  = (*)
