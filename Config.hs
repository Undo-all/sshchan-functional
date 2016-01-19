module Config ( Config(..), readConfig) where

import Text.Read (readEither)
import Graphics.Vty.Attributes

data Config = Config
            { chanName :: String
            , chanHomepageMsg :: String
            , chanDialogAttr :: Attr
            , chanButtonAttr :: Attr
            , chanButtonSelectedAttr :: Attr
            , chanEditAttr :: Attr
            } deriving (Eq, Show, Read)

readConfig :: String -> Either String Config
readConfig xs =
    readEither ("Config {" ++ xs ++ "}") :: Either String Config

