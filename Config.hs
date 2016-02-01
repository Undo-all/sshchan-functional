module Config where

import Text.Read (readEither)
import Graphics.Vty.Attributes

-- The configuration of the chan.
data Config = Config
            { chanName :: String
            , chanUser :: String
            , chanHomepageMsg :: String
            , chanDialogAttr :: Attr
            , chanButtonAttr :: Attr
            , chanButtonSelectedAttr :: Attr
            , chanEditAttr :: Attr
            } deriving (Eq, Show, Read)

-- Reads a configuration from a string.
readConfig :: String -> Either String Config
readConfig xs =
    readEither ("Config {" ++ xs ++ "}") :: Either String Config

