{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database where

import           App
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.ByteString.Char8       as BS
import           Data.Maybe
import           Data.Text                   (Text)
import           Database.Persist.Monad
import           Database.Persist.Postgresql (ConnectionString, Entity, Key,
                                              withPostgresqlPool)

import           Database.Persist.Sql        ((==.))
import           Database.PostgreSQL.Simple  (SqlError)
import           Schema
import           UnliftIO                    (Exception, Typeable, try)



-- Migrates the database.

migrateDb :: ReaderT Config IO ()
migrateDb = do
    conf <- ask
    liftIO $ print conf
    result <- try $ runAction $ runReaderT action conf
    case result of
        Left (e :: SqlError) ->
            error $ show e
        Right _ ->
            return ()
    where
        action = runMigration migrateAll


-- Appends all the fields in Config together to make a ConnectionString.
-- A ConnectionString type is an alias for the ByteString type.

connString :: Config -> ConnectionString
connString env =
    case (dbHost env, dbUrl env) of
        (Just host, _) ->
            "host=" `BS.append` host `BS.append` " \
            \port=5432 \
            \user=postgres \
            \dbname=postgres \
            \password=secretpassword"
        (_, Just url) ->
            url
        _ ->
            error "No DATABASE_URL or DATABASE_HOST given."


-- Run a SQL action in the database.

runAction :: AppM a -> ReaderT Config IO a
runAction action = do
    conf <- ask
    runStdoutLoggingT $
        withPostgresqlPool (connString conf) 5 $
            \pool -> liftIO $ runReaderT (runSqlQueryT pool $ runAppM action) conf



--
-- User actions
--

createAccount :: Account -> UserInfo -> AppM ()
createAccount acc userInfo =
    insert_ acc >> insert_ userInfo



-- TODO: update user info in DB

updateUserInfo :: UserInfo -> AppM ()
updateUserInfo = undefined


getUserInfo :: Key Account -> AppM (Entity UserInfo)
getUserInfo key = do
    mbUserInfo <- selectFirst [UserInfoAccountId ==. key] []
    case mbUserInfo of
        Just userInfo ->
            return userInfo
        Nothing       ->
            error "Bruh moment @ getUserInfo"




submitRegistration :: Registration -> AppM ()
submitRegistration = insert_


getRegistration :: Key Account -> AppM (Entity Registration)
getRegistration key = do
    mbRegistration <- selectFirst [RegistrationAccountId ==. key] []
    case mbRegistration of
        Just registration ->
            return registration
        Nothing ->
            error "Bruh moment @ getRegistration"


--
-- Admin actions
--

getAllAccounts :: AppM [Entity Account]
getAllAccounts =
    selectList [] []


getAllUserInfo :: AppM [Entity UserInfo]
getAllUserInfo =
    selectList [] []


getAllRegistrations :: AppM [Entity Registration]
getAllRegistrations =
    selectList [] []
