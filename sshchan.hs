{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Data.Time
import Data.Maybe
import Data.String
import Data.Text (Text)
import Brick.Widgets.Edit
import Graphics.Vty.Image
import Control.Monad.Trans
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Control.Monad (when)
import Text.Read (readMaybe)
import Database.SQLite.Simple
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Database.SQLite.Simple.FromField

-- Posts hold posts!
data Post = Post (Maybe Text) (Maybe Text) Day Int Text
          deriving (Eq, Show)

-- There's definitely a cleaner way to do this...
instance FromRow Post where
    fromRow = do id      <- field
                 date    <- field
                 subject <- field 
                 name    <- field
                 content <- field
                 return $ Post subject name date id content

-- Threads hold threads!
data Thread = Thread Post [Post]
            deriving (Eq, Show)

-- Converts a Maybe value into a SQL value.
showNull :: Show a => Maybe a -> Text
showNull Nothing  = "NULL"
showNull (Just x) = T.pack (show x)

-- Make a post (ofc)
makePost :: Connection -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Int -> IO ()
makePost conn subject name content board reply = do
    execute_ conn (Query post)
    when (isJust reply) $
      execute_ conn (Query bump)
  where post = T.concat [ "INSERT INTO posts VALUES(NULL,date('now'),datetime('now'),"
                        , showNull subject, ","
                        , showNull name, ","
                        , T.pack (show content), ","
                        , T.pack (show board), ","
                        , showNull reply, ");"
                        ]
        bump = T.concat [ "UPDATE posts SET post_last_bumped = datetime('now')\
                          \WHERE post_id = "
                        , T.pack (show $ fromMaybe undefined reply)
                        ]

-- Fetch the replies to a post (slow?)
getReplies :: Connection -> Int -> Int -> IO [Post]
getReplies conn id board =
    query_ conn (Query queryReplies)
  where queryReplies = T.concat [ "SELECT post_id, post_date, post_by, \
                                  \post_subject, post_content FROM posts WHERE \
                                  \post_reply = "
                                , T.pack (show id)
                                , " AND post_board = "
                                , T.pack (show board)
                                , " ORDER BY post_id ASC"
                                ]

-- Get the threads on a board.
getThreads :: Connection -> Int -> IO [Thread]
getThreads conn board = do
    ops     <- query_ conn (Query queryOps)
    replies <- mapM (\(Post _ _ _ id _) -> getReplies conn id board) ops
    return (zipWith Thread ops replies)
  where queryOps = T.concat [ "SELECT post_id, post_date, post_by, post_subject\
                              \, post_content FROM posts WHERE post_reply IS\
                              \ NULL AND post_board = "
                            , T.pack (show board)
                            , " ORDER BY post_last_bumped DESC"
                            ]

-- Renders a post.
renderPost :: Post -> Widget
renderPost (Post subject name date id content) =
    let renderSubject =
          raw . string (Attr (SetTo bold) (SetTo blue) Default) . T.unpack
        subjectInfo   = maybe emptyWidget renderSubject subject
        renderName    = raw . string (Attr (SetTo bold) (SetTo green) Default)
        nameInfo      = renderName $ maybe "Anonymous" T.unpack name
        dateInfo      = str (showGregorian date)
        idInfo        = str ("No. " ++ show id)
        allInfo       = [subjectInfo, nameInfo, dateInfo, idInfo]
        info          = hBox $ map (padRight (Pad 1)) allInfo
        body          = vBox . map txt . T.splitOn "\\n" $ content
    in border . padRight Max $ padBottom (Pad 1) info <=> body

-- Render several posts.
renderPosts :: [Post] -> Widget
renderPosts = vBox . map renderPost

-- Render a thread.
renderThread :: Thread -> Widget
renderThread (Thread op xs) =
    renderPost op <=> padLeft (Pad 2) (renderPosts xs)

-- Render several threads (these comments are helpful, aren't they?)
renderThreads :: [Thread] -> Widget
renderThreads = viewport "threads" Vertical . vBox . map renderThread

-- This stores what page you're on.
data Page = Homepage (Dialog String)
          | ViewBoard Int [Thread] 
          | MakePost Int PostUI

-- The posting UI, in all it's fanciness.
-- I use an integer to store what editor is currently selected because
-- I don't wanna learn how to use lenses (they're scary).
data PostUI = PostUI Int Editor Editor Editor Editor

-- The default (only) posting UI.
defPostUI :: PostUI
defPostUI = PostUI 0 ed1 ed2 ed3 ed4
  where ed1 = editor "subject" (str . unlines) (Just 1) ""
        ed2 = editor "name" (str . unlines) (Just 1) ""
        ed3 = editor "reply" (str . unlines) (Just 1) ""
        ed4 = editor "content" (str . unlines) Nothing ""

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

-- Render a PostUI.
renderPostUI :: PostUI -> Widget
renderPostUI (PostUI _ ed1 ed2 ed3 ed4) =
    vBox [ (padRight (Pad 120) . padBottom (Pad 1) $ str "Ctrl+S to save") <+>
            padBottom (Pad 1) (str "Ctrl+C to cancel")
         , vLimit 35 . hLimit 150 . padBottom (Pad 1) $ hBox
               [ str "Subject: ", renderEditor ed1
               , padLeft (Pad 1) $ str "Name: ", renderEditor ed2
               , padLeft (Pad 1) $ str "Reply to: ", renderEditor ed3
               ]
         , vLimit 35 . hLimit 150 $ renderEditor ed4
         ]
-- Our application state. The only reason this isn't just Page is because
-- you have to keep track of the sqlite database connection.
data AppState = AppState Page Connection

-- Construct the homepage dialog.
homepageDialog :: Connection -> IO (Dialog String)
homepageDialog conn = do
    name   <- liftIO $ init <$> readFile "name.txt"
    boards <- liftIO $ query_ conn "SELECT board_name FROM boards"
    let xs      = map (\(Only board) -> T.unpack board) boards
        choices = zip xs xs 
    return $ dialog "boardselect" (Just name) (Just (0, choices)) 50

-- Draw the AppState.
drawUI :: AppState -> [Widget]
drawUI (AppState (Homepage d) _) =
    [renderDialog d . hCenter . padAll 1 $ str ""]
drawUI (AppState (ViewBoard _ xs) _) =
    [hCenter instructions <=> renderThreads xs]
  where instructions = hBox . map (padLeftRight 10) $
                           [ str "Ctrl+P to make a post"
                           , str "Ctrl+Z to return to the homepage"
                           , str "Ctrl+R to refresh page"
                           , str "Esc to disconnect"
                           ]
drawUI (AppState (MakePost _ ui) _) = [center $ renderPostUI ui]

-- This handles events, and boy oh boy, does it just do it in the least
-- elegant looking way possible.
appEvent :: AppState -> Event -> EventM (Next AppState)
appEvent st@(AppState (Homepage d) conn) ev =
    case ev of
      EvKey KEsc []   -> halt st
      EvKey KEnter [] ->
        if isNothing (dialogSelection d)
          then continue st
          else do let name = fromMaybe undefined (dialogSelection d) 
                  [Only board] <- liftIO $ query_ conn $ Query $ T.concat
                                    [ "SELECT board_id FROM boards \
                                      \WHERE board_name = "
                                    , T.pack (show name)
                                    ]
                  xs <- liftIO $ getThreads conn board
                  continue (AppState (ViewBoard board xs) conn)
      _               ->
        handleEvent ev d >>= continue . (\d -> AppState (Homepage d) conn)

appEvent st@(AppState (ViewBoard board xs) conn) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey KUp []              -> do
        vScrollBy (viewportScroll "threads") (-5)
        continue st
      EvKey KDown []            -> do
        vScrollBy (viewportScroll "threads") 5
        continue st
      EvKey (KChar 'r') [MCtrl] -> do
        xs' <- liftIO $ getThreads conn board
        continue (AppState (ViewBoard board xs') conn)
      EvKey (KChar 'z') [MCtrl] -> do
        d <- liftIO $ homepageDialog conn
        continue (AppState (Homepage d) conn)
      EvKey (KChar 'p') [MCtrl] ->
        continue (AppState (MakePost board $ defPostUI) conn)
      _                         -> continue st

appEvent st@(AppState (MakePost board ui@(PostUI focus ed1 ed2 ed3 ed4)) conn) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey (KChar 'c') [MCtrl] -> do
        xs <- liftIO $ getThreads conn board
        continue (AppState (ViewBoard board xs) conn)
      EvKey (KChar 's') [MCtrl] ->
        if null (getEditContents ed4) || all null (getEditContents ed4)
          then continue st
          else let
                 subject = T.pack <$> listToMaybe' (head $ getEditContents ed1)
                 name    = T.pack <$> listToMaybe' (head $ getEditContents ed2)
                 reply   = listToMaybe (getEditContents ed3) >>= readInt
                 content = T.pack . unlines $ getEditContents ed4
               in do liftIO $ makePost conn subject name (T.stripEnd content) 
                              board reply
                     xs' <- liftIO $ getThreads conn board 
                     continue (AppState (ViewBoard board xs') conn)
      EvKey (KChar '\t') [] ->
        continue (AppState (MakePost board
                              (PostUI ((focus+1) `mod` 4)
                                      ed1 ed2 ed3 ed4))
                           conn)
      _                         -> do
        ed <- handleEvent ev (currentEditor ui)
        continue (AppState (MakePost board (updateEditor ui ed)) conn)
  where readInt x       = readMaybe x :: Maybe Int
        listToMaybe' [] = Nothing
        listToMaybe' xs = Just xs

-- This dictates where the cursor goes. Note that we don't give a shit
-- unless we're making a post.
appCursor :: AppState -> [CursorLocation] -> Maybe CursorLocation
appCursor (AppState (MakePost _ ui) _) = showCursorNamed (editorName $ currentEditor ui)
appCursor st                           = showFirstCursor st

-- The attribute map.
theMap :: AttrMap
theMap = attrMap defAttr [ (dialogAttr, white `on` blue)
                         , (buttonAttr, black `on` white)
                         , (buttonSelectedAttr, bg yellow)
                         , (editAttr, black `on` white)
                         ]

-- The application itself, in all it's glory.
theApp :: App AppState Event
theApp =
    App { appDraw         = drawUI
        , appChooseCursor = appCursor
        , appHandleEvent  = appEvent
        , appStartEvent   = return
        , appAttrMap      = const theMap
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    name <- init <$> readFile "name.txt"
    conn <- open (name ++ ".db")
    d    <- homepageDialog conn
    defaultMain theApp (AppState (Homepage d) conn)
    return ()

