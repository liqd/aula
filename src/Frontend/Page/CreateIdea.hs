{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.CreateIdea
where

import Frontend.Prelude

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 6. Create idea
data PageCreateIdea = PageCreateIdea
  deriving (Eq, Show, Read)

----------------------------------------------------------------------
-- templates

-- | The page is shown when the idea creation has happened.
instance ToHtml PageCreateIdea where
    toHtmlRaw = toHtml
    toHtml (PageCreateIdea) = do
        p_ $ "The idea has been created."

instance FormPageView PageCreateIdea where
    type FormPageResult PageCreateIdea = ProtoIdea

    makeForm PageCreateIdea =
        ProtoIdea
        <$> ("title"         .: DF.text Nothing)
        <*> ("idea-text"     .: (Markdown <$> DF.text Nothing))
        <*> ("idea-category" .: DF.choice categories Nothing)
        where
          categories = [
                (CatRule,        "Regel")
              , (CatEquipment,   "Ausstattung")
              , (CatClass,       "Unterricht")
              , (CatTime,        "Zeit")
              , (CatEnvironment, "Umgebung")
              ]

    formPage v formAction PageCreateIdea = do
        div_ $ do
            h3_ "Create Idea"
            DF.form v formAction $ do
                DF.inputText      "title" v
                br_ []
                DF.inputTextArea  Nothing Nothing "idea-text" v
                br_ []
                DF.inputSelect    "idea-category" v
                br_ []
                DF.inputSubmit "Add Idea"

instance RedirectOf PageCreateIdea where
    redirectOf _ = "/ideas"

----------------------------------------------------------------------
-- handlers

createIdea :: Server (FormH HTML (Html ()) ST)
createIdea = formRedirectH "/ideas/create" p1 p2 r
  where
    p1 :: DF.Form (Html ()) (ExceptT ServantErr IO) ProtoIdea
    p1 = makeForm PageCreateIdea

    p2 :: ProtoIdea -> ExceptT ServantErr IO ST
    p2 idea = liftIO $ do
        void . runPersist $ do
            forceLogin 1 -- FIXME: Login hack
            addIdea idea
        return $ redirectOf PageCreateIdea

    r :: View (Html ()) -> ST -> ExceptT ServantErr IO (Html ())
    r v formAction = pure . pageFrame $ formPage v formAction PageCreateIdea
