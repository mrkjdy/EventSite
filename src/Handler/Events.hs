{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Events where

import Import

getEventsR :: Handler Html
getEventsR = do
    muser <- maybeAuthPair
    eventList <- runDB $
        case muser of
            Nothing -> selectList 
                [EventEventType ==. PublicEvent] [Asc EventDateTime]
            Just _ ->  selectList 
                (   [EventEventType ==. PublicEvent]
                ||. [EventEventType ==. UniversityEvent]
                ) 
                [Asc EventDateTime]
    defaultLayout $ do
        setTitle "Events"
        $(widgetFile "events")

postEventsR :: Handler Html
postEventsR = error "Not yet implemented: postEventsR"
