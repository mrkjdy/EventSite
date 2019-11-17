{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Events where

import Import

data Event = Event 
    { eventLabel :: Text
    , eventRoute :: Route App
    , eventType :: Text
    , eventDate :: Text
    , eventTime :: Text
    }

testEventList :: [Event]
testEventList = 
    [ Event "This is a test event" HomeR "Test type" "the 10th" "1pm"
    , Event "pp event!" HomeR "pp type" "November pp" "pp pm"
    , Event "Woooooo" ProfileR "hahahha" "Cheenar" "am pm"
    ]

getEventsR :: Handler Html
getEventsR = do
    defaultLayout $ do
        setTitle "Events"
        let eventList = testEventList
        $(widgetFile "events")

postEventsR :: Handler Html
postEventsR = error "Not yet implemented: postEventsR"
