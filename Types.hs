module Types where

import Format
import Data.Time
import Data.Text.Markup
import Data.Text (Text)
import Database.SQLite.Simple.FromRow
import Graphics.Vty.Attributes (Attr)

-- Represents an IP address (or hostname).
type IP = String

-- Posts hold posts!
data Post = Post
          { postSubject :: Maybe Text
          , postBy      :: Maybe Text 
          , postDate    :: Day 
          , postID      :: Int 
          , postContent :: Markup Attr
          } 

-- There's definitely a cleaner way to do this...
instance FromRow Post where
    fromRow = do id      <- field
                 date    <- field
                 subject <- field 
                 name    <- field
                 content <- field
                 return $ Post subject name date id (parseFormat content)

-- Threads hold threads!
data Thread = Thread 
            { threadOP       :: Post 
            , threadReplies  :: [Post]
            , threadOmitted  :: Maybe Int
            , threadStickied :: Bool
            , threadLocked   :: Bool
            } 

