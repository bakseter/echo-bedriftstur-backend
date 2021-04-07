{-# LANGUAGE DeriveGeneric #-}

module SchemaTypes where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Password.Argon2
import           Data.Text                   (pack, unpack)
import           Database.Persist.Postgresql
import           GHC.Generics                (Generic)
import qualified Text.Read                   as T (readMaybe)


-- DEGREE
--
-- Custom type for a students degree.

data Degree
    = DTEK
    | DSIK
    | DVIT
    | BINF
    | IMØ
    | IKT
    | KOGNI
    | INF
    | PROG
    | POST
    | ÅRMNINF
    | MISC
    deriving (Generic, Read, Eq, Show)


instance FromJSON Degree
instance ToJSON Degree


-- How to turn Degree to and from the interal type used by Persist

instance PersistField Degree where
    -- Use show on Degree and wrap it in PersistText

    toPersistValue val = PersistText $ pack $ show val


    -- Unwrap from PersistText and try to read.
    -- Fail if not PersistText.

    fromPersistValue (PersistText t) =
        case T.readMaybe $ unpack t of
            Just p ->
                Right p
            Nothing ->
                Left $ pack "Cannot convert from PersistValue to Degree; failed on read."
    fromPersistValue _ = Left $ pack "Cannot convert from PersistValue to Degree; not PersistText."


-- What SQL datatype the Degree type should be stored as in our database.

instance PersistFieldSql Degree where
    sqlType _ = SqlString
