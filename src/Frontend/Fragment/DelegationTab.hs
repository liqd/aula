{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall    #-}

module Frontend.Fragment.DelegationTab
where

import Data.List (intersperse)
import Frontend.Prelude hiding ((</>), (<.>))
import Persistent
    ( DelegateeListsMap(..)
    , unDelegateeLists
    )
import qualified Frontend.Path as U


temporary :: DelegateeListsMap -> [(User, [User])]  -- TODO: #682
temporary (DelegateeListsMap xs) = concat $ unDelegateeLists . snd <$> xs

renderDelegations :: forall m. Monad m => DelegateeListsMap -> HtmlT m ()
renderDelegations (temporary -> delegations) = do
    ul_ [class_ "small-avatar-list"] $ renderLi `mapM_` delegations
  where
    renderLi :: (User, [User]) -> HtmlT m ()
    renderLi (delegate, delegatees) = do
        li_ [class_ "small-avatar-list-item"] $ do
            div_ [class_ "col-1-12"] $ do
                div_ [class_ "small-avatar-list-image"] $ do
                    nil -- FIXME Make a real image a child here (avatarImgFromHasMeta)
            div_ [class_ "col-11-12"] $ do
                h3_ $ a_ [href_ $ U.viewUserProfile delegate]
                    (delegate ^. userLogin . unUserLogin  . html)
                case length delegatees of
                    0 -> nil
                    n -> do
                        p_ $ do
                            -- FUTUREWORK: we should name the voting power here, and then the list of
                            -- delegatees.  (voting power is different from length of delegatee list unless
                            -- the delegate has delegated to herself.)
                            toHtml $ show (length delegatees)
                                <> " Stimme" <> (if n == 1 then "" else "n")
                                <> " von "
                            let f :: User -> HtmlT m ()
                                f delegatee = strong_ $ a_ [href_ $ U.viewUserProfile delegatee]
                                    (delegatee ^. userLogin . unUserLogin  . html)
                            sequence_ . intersperse ", " $ f <$> delegatees
