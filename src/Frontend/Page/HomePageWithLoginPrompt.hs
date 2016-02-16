{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.HomePageWithLoginPrompt
where

import Frontend.Prelude

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt
  deriving (Eq, Show, Read)


----------------------------------------------------------------------
-- templates

instance ToHtml PageHomeWithLoginPrompt where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageHomeWithLoginPrompt"

instance FormPageView PageHomeWithLoginPrompt where
    type FormPageResult PageHomeWithLoginPrompt = (ST, ST)

    makeForm _ = do
        (,) <$> ("user" .: DF.text Nothing)
            <*> ("pass" .: DF.text Nothing)

    formPage v formAction p = do
        semanticDiv p $ do
            div_ $ DF.form v formAction $ do
                DF.inputText     "user" v >> br_ []
                DF.inputPassword "pass" v >> br_ []
                DF.inputSubmit   "Login"
            div_ $ do
                p_ $ "Solltest du dein Passwort nich mehr kennen, melde dich bitte bei den Admins euer Schule."

----------------------------------------------------------------------
-- handlers

instance RedirectOf PageHomeWithLoginPrompt where
    redirectOf _ = "/ideas"

login :: Server (FormH HTML (Html ()) ST)
login = formRedirectH "/login" p1 p2 r
  where
    p1 :: DF.Form (Html ()) (ExceptT ServantErr IO) (ST, ST)
    p1 = makeForm PageHomeWithLoginPrompt

    p2 :: (ST, ST) -> ExceptT ServantErr IO ST
    p2 (user, _pass) = liftIO $ do
        runPersist $ loginUser user
        return $ redirectOf PageHomeWithLoginPrompt

    r :: View (Html ()) -> ST -> ExceptT ServantErr IO (Html ())
    r v formAction = pure . publicPageFrame $ formPage v formAction PageHomeWithLoginPrompt
