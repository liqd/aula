module AulaPrelude (module X) where

import Control.Applicative         as X
import Control.Exception           as X ( handle, bracket, try, catch, throwIO, evaluate
                                        , ErrorCall(..), SomeException(..), IOException)
import Control.Lens                as X hiding (Bifunctor, Context, bimap, contexts)
import Control.Monad               as X
import Data.Char                   as X (isAlpha, ord, toUpper)
import Control.Monad.Error.Class   as X (MonadError, catchError, throwError)
import Control.Monad.IO.Class      as X (MonadIO (liftIO))
import Control.Monad.Reader.Class  as X (MonadReader (ask, local, reader), asks)
import Control.Monad.State.Class   as X (MonadState (put, get, state), gets, modify, modify')
import Data.Bifunctor              as X (Bifunctor, bimap, first, second)
import Data.Either                 as X (isLeft, isRight)
import Data.Foldable               as X
import Data.Function               as X (on)
import Data.Functor                as X (($>))
import Data.Functor.Infix          as X ((<$$>))
import Data.List                   as X ((\\), nub, nubBy, partition, intersperse, intercalate, sort, sortBy)
import Data.Maybe                  as X (catMaybes, fromJust, fromMaybe, isJust, isNothing, maybeToList)
import Data.Monoid                 as X
import Data.Proxy                  as X (Proxy (Proxy))
import Data.String                 as X (IsString (fromString))
import Data.String.Conversions     as X (ConvertibleStrings(..), LBS, LT, SBS, ST, cs)
import Data.Traversable            as X
import Data.Typeable               as X (Typeable, typeOf, typeRep)
import GHC.Generics                as X (Generic)
import Text.Read                   as X (readMaybe, readEither)
import Text.Show.Pretty            as X (ppShow)
