{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Admin
where

import Frontend.Prelude

----------------------------------------------------------------------
-- types

-- | 11.1 Admin settings: Durations
data PageAdminSettingsDurations =
    PageAdminSettingsDurations
  deriving (Eq, Show, Read)

-- | 11.2 Admin settings: Durations
data PageAdminSettingsQuorum =
    PageAdminSettingsQuorum
  deriving (Eq, Show, Read)

-- | 11.3 Admin settings: Manage groups & permissions
data PageAdminSettingsGroupsAndPermissions =
    PageAdminSettingsGroupsAndPermissions
  deriving (Eq, Show, Read)

{-
-- | 11.3 Admin settings: User creation & user import
data PageAdminSettingsUserCreateAndImport =
    PageAdminSettingsUserCreateAndImport
  deriving (Eq, Show, Read)

instance ToHtml PageAdminSettingsUserCreateAndImport where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsUserCreateAndImport"
-}

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol
  deriving (Eq, Show, Read)

----------------------------------------------------------------------
-- templates

instance ToHtml PageAdminSettingsDurations where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsDurations"

instance ToHtml PageAdminSettingsQuorum where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsQuorum"

instance ToHtml PageAdminSettingsGroupsAndPermissions where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsGroupsAndPermissions"

instance ToHtml PageAdminSettingsEventsProtocol where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageAdminSettingsEventsProtocol"
