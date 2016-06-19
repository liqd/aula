{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -Werror -Wall    #-}

module Frontend.Fragment.DelegationTab
where

import Frontend.Prelude hiding ((</>), (<.>))
import Persistent
    ( DelegateeLists(..)
    )
import qualified Frontend.Path as U


renderDelegations :: forall m. Monad m => Bool -> DelegateeLists -> HtmlT m ()
renderDelegations showTotal (DelegateeLists delegations) = do
    when showTotal $ h2_ ("Insgesamt " <> total ^. showed . html)
    ul_ [class_ "small-avatar-list"] $ renderLi `mapM_` delegations
  where
    total = sum $ map ((1 +) . length . snd) delegations

    renderLi :: (User, [User]) -> HtmlT m ()
    renderLi (delegatee, secondDelegatees) = do
        li_ [class_ "small-avatar-list-item"] $ do
            div_ [class_ "col-1-12"] $ do
                div_ [class_ "small-avatar-list-image"] $ do
                    nil -- FIXME Make a real image a child here (avatarImgFromHasMeta)
            div_ [class_ "col-11-12"] $ do
                h3_ $ a_ [href_ $ U.viewUserProfile delegatee] (delegatee ^. userLogin . unUserLogin  . html)
                p_ $ do
                    toHtml $ show (length secondDelegatees) <> " Stimmen von "
                    strong_ . forM_ secondDelegatees $ \delegatee' ->
                        a_ [href_ $ U.viewUserProfile delegatee'] (delegatee' ^. userLogin . unUserLogin  . html)
