{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Schema where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Password.Argon2
import           Data.Password.Instances
import           Data.Time.Clock         (UTCTime)
import           Database.Persist.TH
import           GHC.Generics            (Generic)
import           SchemaTypes


-- This is the schema for our database tables.
-- It generates our tables as record types.

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

    Account
        email           String
        password        (PasswordHash Argon2)

        UniqueEmail     email

        deriving Show Generic

    UserInfo
        accountId       AccountId
        firstName       String
        lastName        String
        degree          String

        deriving Show Generic


    Registration
        accountId       AccountId
        timestamp       UTCTime Maybe default=CURRENT_TIMESTAMP
        terms           Bool


        deriving Show Generic
|]


-- Makes all the types an instance of FromJSON and ToJSON;
-- this means that our program can convert a JSON object
-- with the structure of a given type to an actual Haskell data type.

instance FromJSON UserInfo
instance ToJSON UserInfo

instance FromJSON Registration
instance ToJSON Registration
