module Environment where

import           App
import           Configuration.Dotenv.Environment (getEnvironment, lookupEnv)
import           Data.Bifunctor
import qualified Data.ByteString.Char8            as BS
import           Data.Maybe
import qualified Data.Text                        as T (pack)
import           System.Environment               (getArgs)


-- Get runtime arguments or environment variables, and
-- (maybe) return a Config.
--
-- Runtime arguments are values given to our program at runtime
-- on the form "key=value", separated by spaces.
--
-- Example:
--     stack exec our-application ARGUMENT1=value1 ARGUMENT2=value2 ...
--
--
-- Environment variables are values either defined in the .env
-- file on the form "key=value", or are environment variables
-- defined in the shell the application is running in.


getConfig :: IO (Maybe Config)
getConfig = do
    args <- keyval <$> getArgs
    env <- getEnvironment
    isDev <- isDevelopment
    case mapMaybe (`lookup` (args ++ env)) envVarKeys of
            db : host : user : password : keys ->
                return $ Just $
                    Config
                        (BS.pack db)
                        (BS.pack host)
                        (BS.pack user)
                        (BS.pack password)
                        isDev
                        (T.pack <$> keys)
            _ ->
                return Nothing
    where
        keyval :: [String] -> [(String, String)]
        keyval xs = map (second tail . span (/= '=')) xs


-- Checks if we are running in an development environment.

isDevelopment :: IO Bool
isDevelopment = isJust <$> lookupEnv "DEVELOPMENT"


-- The keys of our environment variables.

envVarKeys :: [String]
envVarKeys =
    [ "POSTGRES_DB"
    , "POSTGRES_HOST"
    , "POSTGRES_USER"
    , "POSTGRES_PASSWORD"
    ] ++ [ "API_KEY_" ++ show x | x <- ([1..numApiKeys] :: [Integer]) ]
    where
        numApiKeys = 5
