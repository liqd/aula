{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.CreateIdea
where

import Action (Action, persistent)
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

----------------------------------------------------------------------
-- templates

-- | The page is shown when the idea creation has happened.
instance ToHtml PageCreateIdea where
    toHtmlRaw = toHtml
    toHtml (PageCreateIdea) = do
        p_ "The idea has been created."

instance FormPageView PageCreateIdea where
    type FormPageResult PageCreateIdea = ProtoIdea

    makeForm PageCreateIdea =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: DF.choice categoryValues Nothing)

    formPage v formAction p = do
        semanticDiv p $ do
            h3_ "Create Idea"
            DF.form v formAction $ do
                DF.inputText     "title" v >> br_ []
                DF.inputTextArea Nothing Nothing "idea-text" v >> br_ []
                DF.inputSelect   "idea-category" v >> br_ []
                DF.inputSubmit   "Add Idea"

categoryValues :: IsString s => [(Category, s)]
categoryValues = [ (CatRule,        "Regel")
                 , (CatEquipment,   "Ausstattung")
                 , (CatClass,       "Unterricht")
                 , (CatTime,        "Zeit")
                 , (CatEnvironment, "Umgebung")
                 ]

----------------------------------------------------------------------
-- handlers

instance RedirectOf PageCreateIdea where
    redirectOf _ = "/ideas"

createIdea :: ServerT (FormH HTML (Html ()) ST) Action
createIdea = redirectFormHandler "/ideas/create" PageCreateIdea newIdea
  where
    newIdea idea = persistent $ addIdea idea
