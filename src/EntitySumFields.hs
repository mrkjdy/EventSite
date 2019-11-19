{-# LANGUAGE TemplateHaskell #-}
module EntitySumFields where

import Database.Persist.TH
import Prelude

data UserType = Standard | Host | Super
    deriving (Show, Read, Eq)
derivePersistField "UserType"