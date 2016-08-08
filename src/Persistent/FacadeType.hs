module Persistent.FacadeType
where

data PersistMode
    = Query
    | Update

data Persist (m :: PersistMode) a = Persist

instance Functor (Persist m) where
    fmap _f Persist = Persist

instance Applicative (Persist m) where
    pure    = const Persist
    _f <*> _x = Persist

instance Monad (Persist m) where
    return  = pure
    _m >>= _k = Persist
