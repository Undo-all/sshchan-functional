{-# LANGUAGE OverloadedStrings #-}

module Page where

import Types
import Brick
import Config
import Data.Time
import Brick.Widgets.Edit
import Control.Monad.Trans
import Data.Vector (Vector)
import Brick.Widgets.Dialog
import Database.SQLite.Simple
import qualified Data.Text as T

-- This stores what page you're on.
data Page = Homepage (Dialog String)
          | ViewBoard Int (Vector Thread) Int
          | ViewThread Int Int Thread Int
          | MakePost Int PostUI
          | Banned Int (Maybe String) String (Maybe UTCTime)
          | MakeReport Int Int Editor

-- Construct the homepage dialog.
homepageDialog :: Connection -> Config -> IO (Dialog String)
homepageDialog conn Config{ chanName = name, chanHomepageMsg = msg } = do
    boards <- liftIO $ query_ conn "SELECT board_name FROM boards"
    let xs      = map (\(Only board) -> T.unpack board) boards
        choices = zip xs xs 
    return $ dialog "boardselect" (Just name) (Just (0, choices)) 50

-- The posting UI, in all it's fanciness.
-- I use an integer to store what editor is currently selected because
-- I don't wanna learn how to use lenses (they're scary).
data PostUI = PostUI Int Editor Editor Editor Editor

-- Create a PostUI. If passed (Just id), the default text of the "Reply to"
-- editor is that id. If passed (Just reply), the default text of the
-- content editor is ">>reply".
newPostUI :: Maybe Int -> Maybe Int -> PostUI
newPostUI id reply = PostUI 0 ed1 ed2 ed3 ed4
  where ed1       = editor "subject" render (Just 1) ""
        ed2       = editor "name" render (Just 1) ""
        ed3       = editor "reply" render (Just 1) (maybe "" show id)
        ed4       = editor "content" render Nothing (maybe "" mkReply reply)
        render    = str . unlines
        mkReply n = ">>" ++ show n ++ "\n"

-- Get the current (focused) editor of a PostUI.
currentEditor :: PostUI -> Editor
currentEditor (PostUI focus ed1 ed2 ed3 ed4) =
    case focus of
      0 -> ed1
      1 -> ed2
      2 -> ed3
      3 -> ed4

-- Update the current editor of a PostUI.
updateEditor :: PostUI -> Editor -> PostUI
updateEditor (PostUI focus ed1 ed2 ed3 ed4) ed =
    case focus of
      0 -> PostUI focus ed ed2 ed3 ed4
      1 -> PostUI focus ed1 ed ed3 ed4
      2 -> PostUI focus ed1 ed2 ed ed4
      3 -> PostUI focus ed1 ed2 ed3 ed

