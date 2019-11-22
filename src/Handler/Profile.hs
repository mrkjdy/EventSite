{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

data ProfileForm = ProfileForm
    { userType :: UserType
    , firstName :: Text
    , lastName :: Text
    , universityNameForm :: Text
    }

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    -- (formWidget, formEnctype) <- generateFormPost $ profileForm user
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"

-- profileForm :: User -> Form ProfileForm
-- profileForm user = renderBootstrap3 BootstrapBasicForm $ ProfileForm
--     <$> areq (selectFieldList [(Standard, "Standard")
--                               ,(Host, "Host")
--                               ,(Super, "Super")
--                               ]
--              ) "UserType" Nothing
--     <*> aopt textField "First name" Nothing -- fromMaybe "" $ userFirstName user
--     <*> aopt textField (Just "Last name") Nothing -- $ fromMaybe "" $ userLastName user
--     -- <*> areq (selectFieldList universities) "University" Nothing --(Just (userUniversityName user))
--     --     where universities = optionsPersist [] [Asc UniversityName] universityName