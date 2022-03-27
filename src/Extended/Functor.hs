module Extended.Functor 
    ( (<<$>>)
    , module Data.Functor
    ) where

import Data.Functor

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

