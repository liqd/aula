module Debug
where

import Debug.Trace (traceShow)

debug :: (Show x, Monad a) => String -> x -> a ()
debug name x =
    do () <- traceShow (name,x) $ pure ()
       pure ()

debugVal :: (Show x, Monad a) => x -> a x
debugVal x =
    do () <- traceShow x $ pure ()
       pure x
