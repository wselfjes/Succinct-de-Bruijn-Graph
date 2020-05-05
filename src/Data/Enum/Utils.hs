{-# LANGUAGE ScopedTypeVariables #-}
module Data.Enum.Utils where

toBoundedEnum :: forall a. (Bounded a, Enum a) => Int -> a
toBoundedEnum i = toEnum (i + fromEnum (minBound :: a))

fromBoundedEnum :: forall a. (Bounded a, Enum a) => a -> Int
fromBoundedEnum x = fromEnum x - fromEnum (minBound :: a)

boundedEnumSize :: forall a proxy. (Bounded a, Enum a) => proxy a -> Int
boundedEnumSize _ = fromEnum (maxBound :: a) - fromEnum (minBound :: a) + 1
