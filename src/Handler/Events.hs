{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Events where

import Import

-- userRSOIds userId = liftHandler . runDB $
--     map (userRSORsoId . entityVal) $ join $
--     selectList [ UserRSOUserId ==. userId ] []

-- selectUserRSOIds :: UserId -> Handler [Entity RSOId]
-- selectUserRSOIds userId = runDB $ rawSql s [toPersistValue userId]
--     where s = "SELECT ?? FROM user_r_s_o WHERE user_id = ?"

getEventsR :: Handler Html
getEventsR = do
    muser <- maybeAuth
    eventList <- runDB $
        case muser of
            Nothing -> selectList
                [EventEventType ==. PublicEvent] [Asc EventDateTime]
            (Just euser) -> case userUserType $ entityVal euser of
                    Super -> selectList [] [Asc EventDateTime]
                    _ -> selectList
                        (   [ EventEventType ==. PublicEvent ]
                        ||. [ EventEventType ==. UniversityEvent
                            , EventUniversityId ==.
                                (userUniversityId $ entityVal euser)
                            ]
                        -- ||. [ EventEventType ==. RSOEvent
                        --     , EventRsoId <-. (userRSOIds $ entityKey euser)
                        --     ]
                        ) 
                        [Asc EventDateTime]
    defaultLayout $ do
        setTitle "Events"
        $(widgetFile "events")
