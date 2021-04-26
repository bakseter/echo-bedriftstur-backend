{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api
import           App
import           Control.Monad.Reader
import           Data.Maybe
import           Database
import           Environment
import           Error                           ()
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Throttle
import           Servant
import           System.Clock


-- CORS

-- Cross-Origin Resource Sharing lets us make requests
-- to the same IP we are making the request from.
--
-- This is used in development when the frontend and backend
-- projects are both running on the same machine.
--
-- Defining a CORS policy lets us actually do this without
-- the browser blocking every request made by the frontend.
--
-- CORS is not used in production.

corsified :: Middleware
corsified = cors (const $ Just appCorsResourcePolicy)


appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy =
    simpleCorsResourcePolicy
        { corsMethods = ["OPTIONS", "GET", "PUT", "POST"],
          corsRequestHeaders = ["Authorization", "Content-Type"]
        }


-- Gets the middleware; if in dev environment,
-- enable CORS for same origin requests.

getMiddleware :: Bool -> (a -> b) -> (a -> a) -> a -> b
getMiddleware isDev =
    if isDev then
        (.)
    else
        const


-- Define our application.

app :: Config -> Application
app conf = serve api $ hoistServer api (runHandlerM conf) server


main :: IO ()
main = do
    -- Get config from either .env file, environment variables or runtime arguments.
    mbConf <- getConfig
    let conf = fromMaybe (error "Could not read config from neither environment nor runtime arguments.") mbConf

    -- Debug
    when (isDevelopment conf) $ do
        print mbConf
        print $ connString conf

    -- Initialize our throttler. This limits repeated requests from the same IP address.
    throttler <- initThrottler $ defaultThrottleSettings $ TimeSpec 5 0

    -- Get our middleware.
    let mw = getMiddleware (isDevelopment conf) (throttle throttler) corsified

    -- Migrate the database.
    liftIO $ runReaderT migrateDb conf

    -- Run the server.
    run 5000 $ mw $ app conf
