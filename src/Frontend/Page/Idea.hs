{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Idea
  ( PageCreateIdea(..)
  , createIdea
  , PageEditIdea(..)
  , editIdea
  , categoryValues
  )
where

import Action (ActionM, persistent)
import Frontend.Path (Top(TopTesting), path)
import Frontend.Prelude

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 6. Create idea
data PageCreateIdea = PageCreateIdea
  deriving (Eq, Show, Read)

instance Page PageCreateIdea where
  isPrivatePage _ = True

-- | 7. Edit idea
data PageEditIdea = PageEditIdea Idea
  deriving (Eq, Show, Read)

instance Page PageEditIdea where
  isPrivatePage _ = True

----------------------------------------------------------------------
-- templates

categoryValues :: IsString s => [(Category, s)]
categoryValues = [ (CatRule,        "Regel")
                 , (CatEquipment,   "Ausstattung")
                 , (CatClass,       "Unterricht")
                 , (CatTime,        "Zeit")
                 , (CatEnvironment, "Umgebung")
                 ]

ideaFormFields :: Monad m => View (HtmlT m ()) -> HtmlT m ()
ideaFormFields v = do
    DF.inputText     "title" v >> br_ []
    DF.inputTextArea Nothing Nothing "idea-text" v >> br_ []
    DF.inputSelect   "idea-category" v >> br_ []

instance FormPageView PageCreateIdea where
    type FormPageResult PageCreateIdea = ProtoIdea

    formAction _ = path $ TopTesting "/ideas/create"

    makeForm _ =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: DF.choice categoryValues Nothing)

    formPage v fa p = do
        semanticDiv p $ do
            h3_ "Create Idee"
            DF.form v fa $ do
                ideaFormFields v
                DF.inputSubmit   "Add Idea"

instance FormPageView PageEditIdea where
    type FormPageResult PageEditIdea = ProtoIdea

    formAction (PageEditIdea idea) =
        path . TopTesting $ "/ideas/edit/" <> cs (show . (\ (AUID i) -> i) $ idea ^. _Id)

    makeForm (PageEditIdea idea) =
        ProtoIdea
        <$> ("title"         .: DF.text (Just $ idea ^. ideaTitle))
        <*> ("idea-text"     .: (Markdown <$> DF.text (Just . fromMarkdown $ idea ^. ideaDesc)))
        <*> ("idea-category" .: DF.choice categoryValues (Just $ idea ^. ideaCategory))

    formPage v fa p@(PageEditIdea _idea) =
        semanticDiv p $ do
            h3_ "Diene Idee"
            DF.form v fa $ do
                ideaFormFields v
                DF.inputSubmit   "Sriechern"
                button_ [value_ ""] "IDEE LOSCHEN"
                button_ [value_ ""] "Abbrechen"


----------------------------------------------------------------------
-- handlers

instance RedirectOf PageCreateIdea where
    redirectOf _ = path $ TopTesting "/ideas"

createIdea :: (ActionM action) => ServerT (FormH HTML (Html ()) ST) action
createIdea = redirectFormHandler (pure PageCreateIdea) (persistent . addIdea)

instance RedirectOf PageEditIdea where
    redirectOf _ = path $ TopTesting "/ideas"

editIdea :: (ActionM action) => AUID Idea -> ServerT (FormH HTML (Html ()) ST) action
editIdea ideaId =
    redirectFormHandler
        (PageEditIdea . (\ (Just idea) -> idea) <$> persistent (findIdea ideaId))
        (persistent . modifyIdea ideaId . newIdea)
  where
    newIdea protoIdea =   (ideaTitle .~ (protoIdea ^. protoIdeaTitle))
                        . (ideaDesc .~ (protoIdea ^. protoIdeaDesc))
                        . (ideaCategory .~ (protoIdea ^. protoIdeaCategory))
