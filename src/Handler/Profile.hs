{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Profile where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data ProfileForm = ProfileForm
    { userType :: UserType
    , firstName :: Maybe Text
    , lastName :: Maybe Text
    , university :: Entity University
    }

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    (formWidget, formEnctype) <- generateFormPost $
        renderBootstrap3 BootstrapBasicForm $ profileForm user
    defaultLayout $ do
        setTitle . toHtml $ userEmail user <> "'s User page"
        $(widgetFile "profile")

postProfileR :: Handler Html
postProfileR = error "Not yet implemented: postProfileR"

profileForm :: User -> AForm Handler ProfileForm
profileForm user = ProfileForm
    <$> areq (selectFieldList usertypes) "UserType" (userUserType <$> Just user)
    <*> aopt textField "First name" (userFirstName <$> Just user)
    <*> aopt textField "Last name" (userLastName <$> Just user)
    <*> areq (selectField universities) "University" Nothing -- (userUniversity user)
    where
        usertypes :: [(Text, UserType)]
        usertypes = [("Standard", Standard), ("Host", Host), ("Super", Super)]
        -- userUniversity :: User -> Handler (Maybe (Entity University))
        -- userUniversity user = case userUniversityId user of
        --     Nothing -> Nothing
        --     (Just uniid) -> runDB $ case get uniid of
        --         Nothing -> Nothing
        --         (Just (Just unival)) -> (Just (Entity uniid unival))
        universities = optionsPersist [] [Asc UniversityName] universityName