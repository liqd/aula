{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

{-# OPTIONS_GHC -Werror -Wall    #-}

module Frontend.Fragment.DelegationTab
where

import Frontend.Prelude hiding ((</>), (<.>))
import Persistent
    ( EQuery
    , findUser
    , scopeDelegatees
    , findTopic
    , maybe404
    , getVotersForSpace
    )
import qualified Frontend.Path as U


-- Represents two step long delegation paths from a user.
newtype DelegationTree = DelegationTree [(User, [User])]
  deriving (Eq, Show, Read)

-- | Find the delegatees of the given user for the given scope
findDelegatees :: AUID User -> DScope -> EQuery [User]
findDelegatees uid scope = do
    scopeDelegatees uid scope
    >>= mapM (findUser . view delegationFrom)
    >>= pure . catMaybes

-- | Delegation tree for the given user and scope.
-- The first level contains all the delegatees of the given user
userDelegationTree :: AUID User -> DScope -> EQuery DelegationTree
userDelegationTree uid scope = do
    firstLevelDelegatees <- findDelegatees uid scope
    DelegationTree
        <$> forM firstLevelDelegatees
                (\user -> (,) user <$> findDelegatees (user ^. _Id) scope)

-- | Delegation tree for the given scope, the first level contains
-- all the users who can vote in the given topic.
topicDelegationTree :: AUID Topic -> EQuery DelegationTree
topicDelegationTree topicId = do
    let scope = DScopeTopicId topicId
    topic <- maybe404 =<< findTopic topicId
    voters <- getVotersForSpace (topic ^. topicIdeaSpace)
    DelegationTree <$> forM voters (\user ->
                            (,) user <$> findDelegatees (user ^. _Id) scope)

renderDelegations :: forall m. Monad m => Bool -> DelegationTree -> HtmlT m ()
renderDelegations showTotal (DelegationTree delegations) = do
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
