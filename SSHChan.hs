{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Data.Char
import Data.Time
import Data.Maybe
import Data.String
import Data.Monoid
import Brick.Types
import Data.Text (Text)
import System.Unix.Crypt
import Brick.Widgets.Edit
import Graphics.Vty.Image
import Control.Monad.Trans
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Control.Monad (when)
import Database.SQLite.Simple
import qualified Data.Text as T
import Graphics.Vty.Input.Events
import Brick.Widgets.Border.Style
import qualified Data.Vector as V
import Database.SQLite.Simple.FromField
import Text.Read (readMaybe, readEither)

-- Posts hold posts!
data Post = Post
          { postSubject :: Maybe Text
          , postBy :: Maybe Text 
          , postDate :: Day 
          , postID :: Int 
          , postContent :: Text
          } deriving (Eq, Show)

-- There's definitely a cleaner way to do this...
instance FromRow Post where
    fromRow = do id      <- field
                 date    <- field
                 subject <- field 
                 name    <- field
                 content <- field
                 return $ Post subject name date id content

-- Threads hold threads!
data Thread = Thread 
            { threadOP :: Post 
            , threadReplies :: [Post]
            , threadOmitted :: Maybe Int
            } deriving (Eq, Show)

-- Converts a Maybe value into a SQL value.
showNull :: Show a => Maybe a -> Text
showNull Nothing  = "NULL"
showNull (Just x) = T.pack (show x)

-- Code adapted from http://cairnarvon.rotahall.org/2009/01/09/ofioc/

genTripcode :: Maybe Text -> IO (Maybe Text)
genTripcode Nothing = return Nothing
genTripcode (Just xs)
    | isJust (T.find (=='#') xs) =
      let (name, pass) = (\(x,y) -> (x, T.tail y)) . T.breakOn "#" $ xs
      in do trip <- tripcode pass
            return . Just . (T.append (T.concat [name, " !"])) $ trip
    | otherwise              = return (Just xs)
  where tripcode pass = T.pack . (\xs -> drop (length xs - 10) xs) <$>
                            crypt (T.unpack pass) (salt . T.unpack $ pass) 
        salt t = map f . take 2 . tail $ t ++ "H.."
        f c | c `notElem` ['.'..'z'] = '.'
            | c `elem` [':'..'@']    = chr $ ord c + 7
            | c `elem` ['['..'`']    = chr $ ord c + 6
            | otherwise              = c

-- Make a post (ofc)
makePost :: Connection -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Int -> IO ()
makePost conn subject name content board reply = do
    trip <- genTripcode name
    execute conn (Query post) (subject, trip, content, board, reply)
    when (isJust reply) $
      execute_ conn (Query bump)
  where post = "INSERT INTO posts VALUES(NULL,date('now'),datetime('now'),?,?,?,?,?)"
        bump = T.concat [ "UPDATE posts SET post_last_bumped = datetime('now')\
                          \WHERE post_id = "
                        , T.pack (show $ fromMaybe undefined reply)
                        ]

-- Fetch the replies to a post (slow?)
getReplies :: Connection -> Bool -> Int -> Int -> IO [Post]
getReplies conn limited id board = do
    xs <- query_ conn (Query queryReplies)
    return $ if limited then reverse xs else xs
  where base = T.concat [ "SELECT post_id, post_date, post_by, \
                          \post_subject, post_content FROM posts WHERE \
                          \post_reply = "
                         , T.pack (show id)
                         , " AND post_board = "
                         , T.pack (show board)
                         , " ORDER BY post_id"
                        ]
        queryReplies
            | limited = T.concat [ base, " DESC LIMIT 5" ]
            | otherwise = T.concat [ base, " ASC" ]

getThread :: Connection -> Bool -> Int -> Int -> IO Thread
getThread conn limited board id = do
    [op]    <- query_ conn (Query queryOp)
    replies <- getReplies conn limited id board 
    if limited
      then do [Only n] <- query_ conn (Query queryLen)
              if n > 5
                then return (Thread op replies (Just (n-5)))
                else return (Thread op replies Nothing)
      else return (Thread op replies Nothing)
  where queryOp = T.concat [ "SELECT post_id, post_date, post_by, post_subject\
                             \, post_content FROM posts WHERE post_id = "
                           , T.pack (show id)
                           , " AND post_board = "
                           , T.pack (show board)
                           ]
        queryLen = T.concat [ "SELECT count(*) FROM posts WHERE post_reply = "
                            , T.pack (show id)
                            , " AND post_board = "
                            , T.pack (show board)
                            ]

-- Get the threads on a board.
getThreads :: Connection -> Int -> IO [Thread]
getThreads conn board = do
    ops     <- query_ conn (Query queryOps)
    threads <- mapM (getThread conn True board . (\(Only id) -> id)) ops
    return threads
  where queryOps = T.concat [ "SELECT post_id FROM posts WHERE post_reply IS\
                              \ NULL AND post_board = "
                            , T.pack (show board)
                            , " ORDER BY post_last_bumped DESC"
                            ]

-- Renders a post.
renderPost :: Bool -> Post -> Widget
renderPost selected (Post subject name date id content) =
    let renderSubject =
          raw . string (Attr (SetTo bold) (SetTo blue) Default) . T.unpack
        subjectInfo   = maybe emptyWidget renderSubject subject
        renderName    = raw . string (Attr (SetTo bold) (SetTo green) Default)
        nameInfo      = renderName $ maybe "Anonymous" T.unpack name
        dateInfo      = str (showGregorian date)
        idInfo        = str ("No. " ++ show id)
        allInfo       = [subjectInfo, nameInfo, dateInfo, idInfo]
        info          = hBox $ map (padRight (Pad 1)) allInfo
        body          = txt content
        borderStyle   = if selected then unicodeBold else unicode
    in withBorderStyle borderStyle . border $
           padBottom (Pad 1) info <=> padRight (Pad 1) body

-- Render several posts.
renderPosts :: [Post] -> Widget
renderPosts = vBox . map (renderPost False)

-- Render a thread 
renderThread :: Int -> Thread -> Widget
renderThread selected (Thread op xs omitted)
    | selected == (-1) = renderPost False op <=> omitMsg <=>
                         padLeft (Pad 2) (vBox . map (renderPost False) $ xs)
    | selected == 0    = visible $ renderPost True op <=> omitMsg <=>
                         padLeft (Pad 2) (vBox . map (renderPost False) $ xs)
    | otherwise        = renderPost False op <=> omitMsg <=>
                         padLeft (Pad 2) 
                             (vBox . map renderSelected . indexes $ xs)
  where indexes xs = zip xs [0..length xs - 1]
        omitMsg    =
            case omitted of
              Nothing -> emptyWidget
              Just n  -> padLeft (Pad 2) . str $ 
                             show n ++ " posts omitted. Hit enter to view."
        renderSelected (post, index)
            | index + 1 == selected = visible (renderPost True post)
            | otherwise             = renderPost False post

-- Render several threads (these comments are helpful, aren't they?)
renderThreads :: Int -> [Thread] -> Widget
renderThreads selected =
    vBox . map render . indexes
  where indexes xs = zip xs [0..length xs - 1]
        render (thread, index)
            | index == selected = renderThread 0 thread
            | otherwise         = renderThread (-1) thread

-- This stores what page you're on.
data Page = Homepage (Dialog String)
          | ViewBoard Int [Thread] Int
          | ViewThread Int Int Thread Int
          | MakePost Int PostUI

-- In any selection in a series, the index needs to loop around when it is
-- greater than the length of the series. These are helper functions to do
-- that.
selectNext :: Int -> Int -> Int
selectNext x 0 = 0
selectNext x n = (x+1) `mod` n

selectPrev :: Int -> Int -> Int
selectPrev x 0 = 0
selectPrev x n = (x-1) `mod` n

-- The posting UI, in all it's fanciness.
-- I use an integer to store what editor is currently selected because
-- I don't wanna learn how to use lenses (they're scary).
data PostUI = PostUI Int Editor Editor Editor Editor

-- Create a PostUI with optional replies.
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

-- Render a PostUI.
renderPostUI :: PostUI -> Widget
renderPostUI (PostUI _ ed1 ed2 ed3 ed4) =
    vBox [ info, fields, content ]
  where save    = padRight (Pad 120) . padBottom (Pad 1) $ str "Ctrl+S to save"
        cancel  = padBottom (Pad 1) $ str "Ctrl+C to cancel"
        info    = save <+> cancel
        editors = [ str "Subject: ", renderEditor ed1
                  , padLeft (Pad 1) $ str "Name: ", renderEditor ed2
                  , padLeft (Pad 1) $ str "Reply to: ", renderEditor ed3
                  ] 
        fields  = vLimit 35 . hLimit 150 . padBottom (Pad 1) . hBox $ editors
        content = vLimit 35 . hLimit 150 $ renderEditor ed4

-- The configuration of the chan.
data Config = Config
            { chanName :: String
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

-- Our application state. The only reason this isn't just Page is because
-- you have to keep track of the sqlite database connection and the
-- configuration.
data AppState = AppState Connection Config Page

-- Construct the homepage dialog.
homepageDialog :: Connection -> Config -> IO (Dialog String)
homepageDialog conn Config{ chanName = name, chanHomepageMsg = msg } = do
    boards <- liftIO $ query_ conn "SELECT board_name FROM boards"
    let xs      = map (\(Only board) -> T.unpack board) boards
        choices = zip xs xs 
    return $ dialog "boardselect" (Just name) (Just (0, choices)) 50

instructions :: Widget
instructions = hBox . map (padLeftRight 10) $
                   [ str "Ctrl+P to make a post"
                   , str "Ctrl+Z to go back"
                   , str "Ctrl+R to refresh page"
                   , str "Esc to disconnect"
                   ]

-- Draw the AppState.
drawUI :: AppState -> [Widget]
drawUI (AppState _ Config{ chanHomepageMsg = msg } (Homepage d)) =
    [renderDialog d . hCenter . padAll 1 $ str msg]
drawUI (AppState _ _ (ViewBoard _ xs selected)) =
    [ hCenter instructions <=> 
      viewport "threads" Vertical (renderThreads selected xs)
    ]
drawUI (AppState _ _ (ViewThread _ id thread selected)) =
    [ hCenter instructions <=>
      viewport "thread" Vertical (renderThread selected thread) <=>
      (hCenter . str $ "Viewing thread No. " ++ show id)
    ]
drawUI (AppState _ _ (MakePost _ ui)) = [center $ renderPostUI ui]

-- This handles events, and boy oh boy, does it just do it in the least
-- elegant looking way possible.
appEvent :: AppState -> Event -> EventM (Next AppState)
appEvent st@(AppState conn cfg (Homepage d)) ev =
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
                  continue (AppState conn cfg (ViewBoard board xs 0))
      _               ->
        handleEvent ev d >>= continue . AppState conn cfg . Homepage

appEvent st@(AppState conn cfg (ViewBoard board xs selected)) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey KHome []            ->
        continue (AppState conn cfg (ViewBoard board xs 0))
      EvKey KEnd []             ->
        continue (AppState conn cfg (ViewBoard board xs len))
      EvKey KDown []            -> 
        continue $ AppState conn cfg
                            (ViewBoard board xs (selectNext selected len))
      EvKey KUp []              -> 
        continue $ AppState conn cfg 
                            (ViewBoard board xs (selectPrev selected len))
      EvKey KEnter []           -> do
        let id = postID . threadOP $ xs !! selected
        thread <- liftIO $ getThread conn False board id 
        liftIO $ writeFile "thread.txt" (show thread)
        continue (AppState conn cfg (ViewThread board id thread 0))
      EvKey (KChar 'r') [MCtrl] -> do
        xs' <- liftIO $ getThreads conn board 
        continue (AppState conn cfg (ViewBoard board xs' 0))
      EvKey (KChar 'z') [MCtrl] -> do
        d <- liftIO $ homepageDialog conn cfg
        continue (AppState conn cfg (Homepage d))
      EvKey (KChar 'p') [MCtrl] ->
        continue $ AppState conn cfg
                       (MakePost board $ newPostUI Nothing Nothing)
      _                         -> continue st
  where len = length xs

appEvent st@(AppState conn cfg (ViewThread board id thread selected)) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey KHome []            ->
        continue (AppState conn cfg (ViewThread board id thread 0))
      EvKey KEnd []             ->
        continue (AppState conn cfg (ViewThread board id thread len))
      EvKey KUp []              -> 
        continue $ AppState conn cfg
                       (ViewThread board id thread (selectPrev selected len))
      EvKey KDown []            ->
        continue $ AppState conn cfg
                       (ViewThread board id thread (selectNext selected len))
      EvKey (KChar 'r') [MCtrl] -> do
        thread' <- liftIO $ getThread conn False board id 
        continue (AppState conn cfg (ViewThread board id thread' selected))
      EvKey (KChar 'z') [MCtrl] -> do
        xs <- liftIO $ getThreads conn board 
        continue (AppState conn cfg (ViewBoard board xs 0))
      EvKey (KChar 'p') [MCtrl] -> 
        continue $ AppState conn cfg 
                       (MakePost board (newPostUI (Just id) Nothing))
      EvKey KEnter []           -> do
        let reply = if selected == 0
                      then Nothing
                      else Just $ postID (threadReplies thread !! (selected-1))
        continue (AppState conn cfg (MakePost board $ newPostUI (Just id) reply))
      _                         -> continue st
  where len = length (threadReplies thread) + 1

appEvent st@(AppState conn cfg (MakePost board ui@(PostUI focus ed1 ed2 ed3 ed4))) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey (KChar 'c') [MCtrl] -> do
        xs <- liftIO $ getThreads conn board 
        continue (AppState conn cfg (ViewBoard board xs 0))
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
                     continue (AppState conn cfg (ViewBoard board xs' 0))
      EvKey (KChar '\t') [] ->
        continue $ AppState
                     conn cfg
                     (MakePost board (PostUI ((focus+1) `mod` 4)
                                     ed1 ed2 ed3 ed4))
      _                         -> do
        ed <- handleEvent ev (currentEditor ui)
        continue (AppState conn cfg (MakePost board (updateEditor ui ed)))
  where readInt x       = readMaybe x :: Maybe Int
        listToMaybe' [] = Nothing
        listToMaybe' xs = Just xs

-- This dictates where the cursor goes. Note that we don't give a shit
-- unless we're making a post.
appCursor :: AppState -> [CursorLocation] -> Maybe CursorLocation
appCursor (AppState _ _ (MakePost _ ui)) =
    showCursorNamed (editorName $ currentEditor ui)
appCursor st                             = showFirstCursor st

-- Generate the map from a configuration.
makeMap :: Config -> AttrMap
makeMap cfg = attrMap defAttr 
                  [ (dialogAttr, chanDialogAttr cfg)
                  , (buttonAttr, chanButtonAttr cfg)
                  , (buttonSelectedAttr, chanButtonSelectedAttr cfg)
                  , (editAttr, chanEditAttr cfg)
                  ]

-- The application itself, in all it's glory.
makeApp :: Config -> App AppState Event
makeApp cfg =
    App { appDraw         = drawUI
        , appChooseCursor = appCursor
        , appHandleEvent  = appEvent
        , appStartEvent   = return
        , appAttrMap      = const (makeMap cfg)
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    conn <- open "chan.db"
    cfg  <- readConfig <$> readFile "chan.cfg"
    case cfg of
      Left err  -> putStrLn $ "Error parsing config file " ++ err
      Right cfg -> do
          d    <- homepageDialog conn cfg
          defaultMain (makeApp cfg) (AppState conn cfg (Homepage d))
          return ()

