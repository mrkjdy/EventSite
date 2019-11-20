{-# LANGUAGE TemplateHaskell #-}
module EntitySumFields where

import Database.Persist.TH
import Prelude

data UserType = Standard | Host | Super
    deriving (Show, Read, Eq)
derivePersistField "UserType"

data EventType = PublicEvent | UniversityEvent | RSOEvent
    deriving (Show, Read, Eq)
derivePersistField "EventType"