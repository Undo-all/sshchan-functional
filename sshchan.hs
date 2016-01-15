{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Network
import Data.Maybe
import Data.String
import Data.Text (Text)
import Brick.Widgets.Border
import Database.SQLite.Simple
import qualified Data.Text as T
import Database.SQLite.Simple.FromField

data Post = Post Int (Maybe Text) (Maybe Text) Text
          deriving (Eq, Show)

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field

data Thread = Thread Post [Post] (Maybe Int)
            deriving (Eq, Show)

showNull :: Show a => Maybe a -> Text
showNull Nothing  = "NULL"
showNull (Just x) = T.pack (show x)

makeBoard :: Connection -> Text -> Text -> IO ()
makeBoard conn name desc = execute_ conn (Query board)
  where board = T.concat [ "INSERT INTO boards VALUES(NULL,\""
                         , name, "\",\""
                         , desc, "\");"
                         ]

makePost :: Connection -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Int -> IO ()
makePost conn subject name content board reply = execute_ conn (Query post)
  where post = T.concat [ "INSERT INTO posts VALUES(NULL,"
                        , showNull subject, ","
                        , showNull name, ","
                        , T.pack (show content), ","
                        , T.pack (show board), ","
                        , showNull reply, ");"
                        ]

getReplies :: Connection -> Int -> Int -> IO [Post]
getReplies conn id board =
    query_ conn $ Query $ T.concat ["SELECT post_id, post_by, post_subject, post_content FROM posts WHERE post_reply = ", T.pack (show id), " AND post_board = ", T.pack (show board), " ORDER BY post_id ASC"]

main :: IO ()
main = defaultMain (App (\x -> if x == 0 then str "0" else str "1"))

