{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Api where

import           App
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BSLI
import           Data.Password.Argon2
import           Data.Text                     (pack)
import qualified Database                      as DB
import           Database.Persist.Postgresql   (entityVal)
import           Database.PostgreSQL.Simple    (SqlError, sqlErrorMsg)
import           GHC.Generics
import           Schema
import           Servant
import           UnliftIO                      (Exception, MonadUnliftIO, try,
                                                withRunInIO, wrappedWithRunInIO)

data UnsafeAccount = UnsafeAccount
    { uaEmail    :: String
    , uaPassword :: String
    } deriving Generic

instance FromJSON UnsafeAccount
instance ToJSON UnsafeAccount

data CompleteAccount = CompleteAccount
    { caUnsafeAccount :: UnsafeAccount
    , caUserInfo      :: UserInfo
    } deriving Generic

instance FromJSON CompleteAccount
instance ToJSON CompleteAccount


-- API


-- The endpoints of our API.

type API =
        "user"      :>  (   "account"       :>  (ReqBody' '[Required] '[JSON] CompleteAccount   :> Post '[JSON] NoContent )
                    :<|>    "userInfo"      :>  (ReqBody' '[Required] '[JSON] UserInfo          :> Put '[JSON] NoContent
                                           {- :<|>           TODO: authentication          -} :<|> Get  '[JSON] UserInfo
                                                )
                    :<|>    "registration"  :>  (ReqBody' '[Required] '[JSON] Registration      :> Post '[JSON] NoContent
                                           {- :<|>          TODO: authentication           -} :<|> Get  '[JSON] Registration
                                                )
                        )
    :<|> "admin"    :>  (   "account"       :> (Header "Authorization" String        :> Get '[JSON] [UnsafeAccount] )
                    :<|>    "userInfo"      :> (Header "Authorization" String        :> Get '[JSON] [UserInfo]      )
                    :<|>    "registration"  :> (Header "Authorization" String        :> Get '[JSON] [Registration]  )
                        )


-- This is needed for some reason.

api :: Proxy API
api = Proxy


-- The monad stack used by our API endpoints.

type HandlerM = ReaderT Config Handler


-- Turn our HandlerM stack into the Handler monad.

runHandlerM :: Config -> HandlerM a -> Handler a
runHandlerM conf rt = runReaderT rt conf


-- Combine all the endpoints.

server :: ServerT API HandlerM
server =
    (createAccount
    :<|>    (updateUserInfo     :<|> getUserInfo)
    :<|>    (submitRegistration :<|> getRegistration)

    ) :<|>
    (getAllAccounts :<|> getAllUserInfo :<|> getAllRegistrations)



-- HANDLERS
--
-- These are the handlers for each endpoint of our API.
-- They all use the HandlerM monad stack; this gives us the
-- Reader monad where our config is stored.
--
-- Example of return types:
--    A type of `HandlerM (Key Registration)` responds with `Key Registration`
--    to the HTTP request. A type of `HandlerM NoContent` responds with nothing.


createAccount :: CompleteAccount -> HandlerM NoContent
createAccount (CompleteAccount (UnsafeAccount email unsafePwd) userInfo) = do
    safePwd <- hashPassword $ mkPassword $ pack unsafePwd
    let acc = Account email safePwd
    result <- runInHandlerM $ DB.createAccount acc userInfo
    case result of
        Right _ ->
            return NoContent
        Left (e :: SqlError) ->
           throwError $ err500 {errBody = toErrBody e}



updateUserInfo :: UserInfo -> HandlerM NoContent
updateUserInfo userInfo = do
    result <- runInHandlerM $ DB.updateUserInfo userInfo
    case result of
        Right _ ->
            -- Return nothing.
            return NoContent
        Left (e :: SqlError) ->
            throwError $ err500 {errBody = toErrBody e}


getUserInfo :: HandlerM UserInfo
getUserInfo = undefined


submitRegistration :: Registration -> HandlerM NoContent
submitRegistration reg = do
    result <- runInHandlerM $ DB.submitRegistration reg
    case result of
        Right _ ->
            -- Return nothing.
            return NoContent
        Left (e :: SqlError) ->
            throwError $ err500 {errBody = toErrBody e}


getRegistration :: HandlerM Registration
getRegistration = undefined


getAllAccounts :: Maybe String -> HandlerM [UnsafeAccount]
getAllAccounts _ = do
    result <- runInHandlerM DB.getAllAccounts
    case result of
        Right accounts ->
            -- Map entityVal over accounts to extract values from internal Persist values.
            return $ toUnsafeAcc . entityVal <$> accounts
        Left (e :: SqlError) ->
            throwError $ err500 { errBody = toErrBody e }


toUnsafeAcc :: Account -> UnsafeAccount
toUnsafeAcc (Account email _) = UnsafeAccount email "***"


getAllUserInfo :: Maybe String -> HandlerM [UserInfo]
getAllUserInfo _ = do
    result <- runInHandlerM DB.getAllUserInfo
    case result of
        Right userInfo ->
            -- Map entityVal over userInfo to extract values from internal Persist values.
            return $ entityVal <$> userInfo
        Left (e :: SqlError) ->
            throwError $ err500 {errBody = toErrBody e}


getAllRegistrations :: Maybe String -> HandlerM [Registration]
getAllRegistrations _ = do
    result <- runInHandlerM DB.getAllRegistrations
    case result of
        Right regs ->
            -- Map entityVal over regs to extract values from internal Persist values.
            return $ entityVal <$> regs
        Left (e :: SqlError) ->
            throwError $ err500 {errBody = toErrBody e}



runInHandlerM :: (MonadIO m, Exception e, MonadReader Config m) => AppM a -> m (Either e a)
runInHandlerM query = do
    conf <- ask
    liftIO $ try $ runReaderT (DB.runAction query) conf


-- Turns a SqlError into an (nicer) error message.

toErrBody :: SqlError -> BSLI.ByteString
toErrBody err = BSLI.packChars $ BS.unpack $ sqlErrorMsg err
