{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -Werror -Wall    #-}

module Frontend.Fragment.DelegationTab
where

import Frontend.Prelude hiding ((</>), (<.>))
import Persistent (EQuery, findUser, scopeDelegatees)

import qualified Frontend.Path as U


-- Represents two step long delegation paths from a user.
newtype DelegationTree = DelegationTree [(User, [User])]
  deriving (Eq, Show, Read)

delegationTree :: AUID User -> DScope -> EQuery DelegationTree
delegationTree uid scope = do
    let findDelegatees uid' = do
            scopeDelegatees uid' scope
            >>= mapM (findUser . view delegationFrom)
            >>= pure . catMaybes

    firstLevelDelegatees <- findDelegatees uid
    DelegationTree <$> forM firstLevelDelegatees (\user ->
                            (,) user <$> findDelegatees (user ^. _Id))

renderDelegations :: forall m. Monad m => DelegationTree -> HtmlT m ()
renderDelegations (DelegationTree delegations) = do
    h2_ $ "Insgesamt " <> total ^. showed . html
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
