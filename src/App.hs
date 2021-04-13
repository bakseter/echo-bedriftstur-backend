{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module App where

import           Control.Monad.Reader
import           Data.ByteString.Char8  as BS
import           Data.Text
import           Database.Persist.Monad
import           UnliftIO               hiding (Handler)


-- The type for our app configuration.
-- ByteString is more or less the same as a normal String.

data Config = Config
    { dbUrl   :: Maybe BS.ByteString
    , apiKeys :: [Text]
    }
    deriving (Eq, Show)

newtype AppM a
    = AppM { runAppM :: SqlQueryT (ReaderT Config IO) a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadSqlQuery)


instance MonadUnliftIO AppM where
    withRunInIO = wrappedWithRunInIO AppM runAppM
