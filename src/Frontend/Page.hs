{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page (module P) where

import Frontend.Page.Admin      as P
import Frontend.Page.Comment    as P
import Frontend.Page.Delegation as P
import Frontend.Page.FileUpload as P
import Frontend.Page.Idea       as P
import Frontend.Page.Login      as P
import Frontend.Page.Overview   as P
import Frontend.Page.Static     as P
import Frontend.Page.Topic      as P
import Frontend.Page.User       as P
