module HKD.EmptyData where

import GHC.Generics
    ( Generic(to, Rep), U1(..), K1(K1), M1(M1), type (:*:)(..) )

class EmptyData a where
  emptyData :: a
  default emptyData :: Generic a => EmptyDataGeneric (Rep a) => a
  emptyData = to edg

class EmptyDataGeneric a where
  edg :: a b

instance (EmptyDataGeneric a, EmptyDataGeneric b) => EmptyDataGeneric (a :*: b) where
  edg = edg :*: edg

instance EmptyDataGeneric U1 where
  edg = U1

instance EmptyDataGeneric a => EmptyDataGeneric (M1 i c a) where
  edg = M1 edg

instance EmptyDataGeneric (K1 i (Maybe a)) where
  edg = K1 Nothing
