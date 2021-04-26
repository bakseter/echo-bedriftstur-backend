module Environment where

import           App
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Text             as T (pack)
import           System.Environment    (getArgs, getEnvironment)


--
-- Get runtime arguments and/or environment variables, and
-- (maybe) return a Config.
--
-- Runtime arguments are values given to our program at runtime
-- on the form "key=value", separated by spaces.
--
-- Example(s):
--
--     stack exec our-application ARGUMENT1=value1 ARGUMENT2=value2 ...
--
--     docker run our-container:latest -e ARGUMENT1=value1 -e ARGUMENT2=value2 ...
--
-- Environment variables are values defined in the shell
-- the application is running in.
--

getConfig :: IO (Maybe Config)
getConfig = do
    args <- keyval <$> getArgs
    env <- getEnvironment
    case map (`lookup` (args ++ env)) envVarKeys of
            mbDbUrl : keys ->
                return $ Just $
                    Config
                        (BS.pack <$> mbDbUrl)
                        (T.pack <$> catMaybes keys)
    where
        keyval :: [String] -> [(String, String)]
        keyval xs = map (second tail . span (/= '=')) xs


-- Checks if we are running in an development environment
-- (aka dbUrl is not defined).

isDevelopment :: Config -> Bool
isDevelopment = isNothing . dbUrl


-- The keys of our environment variables/runtime arguments.

envVarKeys :: [String]
envVarKeys =
    "DATABASE_URL" : [ "API_KEY_" ++ show x | x <- ([1..numApiKeys] :: [Integer]) ]
    where
        numApiKeys = 5
