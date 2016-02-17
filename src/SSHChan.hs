{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Page
import Util
import Wrap
import Brick
import Types
import Config
import Format
import Render
import Data.List
import Data.Time
import Data.Maybe
import Data.Text (Text)
import Brick.Widgets.Edit
import Graphics.Vty.Input
import Control.Monad.Trans
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Dialog
import Data.Vector (Vector)
import Database.SQLite.Simple
import Graphics.Vty.Attributes
import qualified Data.Text as T
import qualified Data.Vector as V
import Text.Read (readMaybe, readEither)
import System.Process (shell, readCreateProcess)

selectNext :: Int -> Int -> Int
selectNext x 0 = 0
selectNext x n = (x+1) `mod` n

selectPrev :: Int -> Int -> Int
selectPrev x 0 = 0
selectPrev x n = (x-1) `mod` n

-- Our application state. The only reason this isn't just Page is because
-- you have to keep track of the sqlite database connection and the
-- configuration.
data AppState = AppState Connection IP Config Page

-- Takes a list of instructions, and makes a widget that equally spaces
-- them, meant to be placed at the top or bottom of the screen.
makeInstructions :: [String] -> Widget
makeInstructions = (<=> hBorder) . hBox . pad . map str
  where pad xs = intersperse space $ space : xs ++ [space]
        space  = vLimit 1 $ fill ' '

-- Draw the AppState.
drawUI :: AppState -> [Widget]
drawUI (AppState _ _ Config{ chanHomepageMsg = msg } (Homepage b d)) =
    [ hCenter (padRight (Pad 1) (str b)) <=>
      vLimit 3 (renderDialog d emptyWidget) <=>
      hCenter (padBottom (Pad 2) $ str "Tab to select board") <=>
      hCenter (vBox $ map (hCenter . str) (lines msg))
    ]

drawUI (AppState _ _ _ (ViewBoard _ name desc xs selected)) =
    [ hCenter instructions <=>
      viewport "threads" Vertical (renderThreads selected xs) <=>
      hCenter (str ("/" ++ name ++ "/ - " ++ desc))
    ]
  where instructions = makeInstructions 
                           [ "Ctrl+P to make a post"
                           , "Ctrl+Z to go back"
                           , "F5 to refresh page"
                           , "Esc to disconnect"
                           ]

drawUI (AppState _ _ _ (ViewThread _ id thread selected)) =
    [ padBottom (Pad 1) (hCenter instructions) <=>
      viewport "thread" Both (renderThread selected thread) <=>
      (hCenter . str $ "Viewing thread No. " ++ show id)
    ]
  where instructions = makeInstructions 
                           [ "Enter to reply"
                           , "Ctrl+Z to go back"
                           , "F5 to refresh page"
                           , "Ctrl+R to report post"
                           , "ESC to disconnect"
                           ]

drawUI (AppState _ _ _ (MakePost _ ui)) = [center $ renderPostUI ui]

drawUI (AppState _ ip _ (Banned _ from reason time)) =
    [ center . borderWithLabel (str "You have been banned!") . padAll 2 $
      (hCenter . str $ "You have been banned from " ++ showFrom from) <=>
      (hCenter . str $ "This ban will expire: " ++ maybe "never" showTime time) <=>
      (hCenter . str $ "Reason: " ++ reason) <=>
      (hCenter . str $ "Your IP: " ++ ip) <=>
      (hCenter . str $ "Press enter to continue.")
    ]
  where showTime = (++" UTC") . show
        showFrom = fromMaybe "all boards"

drawUI (AppState _ ip _ (MakeReport board id ed)) = 
    [ hCenter (hLimit 100 $ save <+> cancel) <=>
      center (hCenter (str $ "Reporting post " ++ show id ++ ". Reason:") <=>
              hCenter (vLimit 25 . hLimit 100 $ renderEditor ed))
    ]
  where save    = padRight Max . padBottom (Pad 1) $ str "Ctrl+S to save"
        cancel  = padBottom (Pad 1) $ str "Ctrl+Z to go back"

-- Generate a ViewBoard page.
viewBoard :: Connection -> Int -> IO Page
viewBoard conn board = do
    xs <- getThreads conn board
    [(name, desc)] <- query conn queryInfo (Only board)
    return $ ViewBoard board name desc xs 0
  where queryInfo = "SELECT board_name, board_description \
                    \FROM boards WHERE board_id = ?"

-- Generate a ViewThread page.
viewThread :: Connection -> Int -> Int -> IO Page
viewThread conn board id = do
    thread <- getThread conn False board id
    return (ViewThread board id thread 0)

-- This handles events, and boy oh boy, does it just do it in the least
-- elegant looking way possible.
appEvent :: AppState -> Event -> EventM (Next AppState)
appEvent st@(AppState conn ip cfg (Homepage b d)) ev =
    case ev of
      EvKey KEsc []   -> halt st
      EvKey KEnter [] ->
        case dialogSelection d of
          Nothing   -> continue st
          Just name -> do
              board <- liftIO $ getBoardID conn name
              page  <- liftIO $ viewBoard conn board
              continue (AppState conn ip cfg page)
      _               -> do d' <- handleEvent ev d
                            continue (AppState conn ip cfg (Homepage b d'))

appEvent st@(AppState conn ip cfg (ViewBoard board n d xs selected)) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey KHome []            ->
        continue (AppState conn ip cfg (ViewBoard board n d xs 0))
      EvKey KEnd []             ->
        continue (AppState conn ip cfg (ViewBoard board n d xs len))
      EvKey KDown []            -> 
        continue $ AppState conn ip cfg
                       (ViewBoard board n d xs (selectNext selected len))
      EvKey KUp []              -> 
        continue $ AppState conn ip cfg 
                       (ViewBoard board n d xs (selectPrev selected len))
      EvKey KEnter []           -> do
        let id = postID . threadOP $ xs V.! selected
        page <- liftIO $ viewThread conn board id
        continue (AppState conn ip cfg page)
      EvKey (KFun 5) []         -> do
        page <- liftIO $ viewBoard conn board 
        continue (AppState conn ip cfg page)
      EvKey (KChar 'z') [MCtrl] -> do
        h <- liftIO $ homepage conn cfg
        continue (AppState conn ip cfg h)
      EvKey (KChar 'p') [MCtrl] ->
        continue $ AppState conn ip cfg
                       (MakePost board $ newPostUI Nothing Nothing)
      _                         -> continue st
  where len = length xs

appEvent st@(AppState conn ip cfg vt@(ViewThread board id thread selected)) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey KHome []            ->
        continue (AppState conn ip cfg (ViewThread board id thread 0))
      EvKey KEnd []             ->
        continue (AppState conn ip cfg (ViewThread board id thread len))
      EvKey KUp []              -> 
        continue $ AppState conn ip cfg
                       (ViewThread board id thread (selectPrev selected len))
      EvKey KDown []            ->
        continue $ AppState conn ip cfg
                       (ViewThread board id thread (selectNext selected len))
      EvKey (KFun 5) []         -> do
        page <- liftIO $ viewThread conn board id
        continue (AppState conn ip cfg page)
      EvKey (KChar 'r') [MCtrl] -> do
        let reportID = if selected == 0
                         then id
                         else postID (threadReplies thread V.! (selected-1))
            ed       = editor "report" (str . unlines) Nothing ""
        continue $ AppState conn ip cfg
                       (MakeReport board reportID ed)
      EvKey (KChar 'z') [MCtrl] -> do
        page <- liftIO $ viewBoard conn board
        continue (AppState conn ip cfg page)
      EvKey KEnter []           -> do
        let reply = if selected == 0
                      then Nothing
                      else Just $ postID (threadReplies thread V.! (selected-1))
            page  = if threadLocked thread
                      then vt
                      else MakePost board $ newPostUI (Just id) reply
        continue $ AppState conn ip cfg page
      _                         -> continue st
  where len = length (threadReplies thread) + 1

-- Warning: absolutely vomit-inducing code (I promise I'll do code cleanup
-- soon)
appEvent st@(AppState conn ip cfg (MakePost board ui@(PostUI focus ed1 ed2 ed3 ed4))) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey (KChar 'z') [MCtrl] -> do
        page <- liftIO $ viewBoard conn board
        continue (AppState conn ip cfg page)
      EvKey (KChar 's') [MCtrl] -> do
        banned <- liftIO $ query_ conn "SELECT * FROM bans"
        case find (\(n, _, _, _) -> n == ip) banned of
          Just (_, from, reason, time) -> 
            case from of
              Nothing   -> ban Nothing reason time 
              Just from -> if from == board then ban (Just board) reason time
                                            else post
          Nothing                      -> post
      EvKey (KChar '\t') []      ->
        continue $ AppState
                     conn ip cfg
                     (MakePost board (PostUI ((focus+1) `mod` 4)
                                     ed1 ed2 ed3 ed4))
      _                         -> do
        ed <- handleEvent ev (currentEditor ui)
        continue (AppState conn ip cfg (MakePost board (updateEditor ui ed)))
  where readInt x            = readMaybe x :: Maybe Int
        listToMaybe' []      = Nothing
        listToMaybe' xs      = Just xs
        ban from reason time = do
            currTime <- liftIO getCurrentTime
            if isJust time && currTime > fromMaybe undefined time
              then do liftIO (execute conn queryRemoveBan (Only . show $ ip))
                      post
              else case from of
                     Nothing   ->
                       continue (AppState conn ip cfg (Banned board Nothing reason time))
                     Just from -> do
                       name <- liftIO $ getBoardName conn from
                       continue (AppState conn ip cfg (Banned board (Just name) reason time))

        post | null (getEditContents ed4) || all null (getEditContents ed4) = 
               continue st
             | otherwise =
               let
                 reply   = listToMaybe (getEditContents ed3) >>= readInt
                 subject = T.pack <$> listToMaybe' (head $ getEditContents ed1)
                 name    = T.pack <$> listToMaybe' (head $ getEditContents ed2)
                 content = T.pack . unlines $ getEditContents ed4
               in do n <- case reply of
                            Nothing -> return Nothing
                            Just x  -> liftIO $ Just <$> isThreadLocked conn board x
                     case n of
                       Just True -> continue st
                       _ -> do
                         liftIO $ makePost conn ip subject name (T.stripEnd content)
                                  board reply
                         page <- liftIO $ viewBoard conn board 
                         continue (AppState conn ip cfg page)
        queryRemoveBan  = "DELETE * FROM bans WHERE ban_ip = ?"

appEvent st@(AppState conn ip cfg (Banned board _ _ _)) ev =
    case ev of
      EvKey KEsc []   -> halt st
      EvKey KEnter [] -> do
        page <- liftIO $ viewBoard conn board
        continue (AppState conn ip cfg page)

appEvent st@(AppState conn ip cfg (MakeReport board id ed)) ev =
    case ev of
      EvKey KEsc []             -> halt st
      EvKey (KChar 'z') [MCtrl] -> do
          page <- liftIO $ viewBoard conn board
          continue (AppState conn ip cfg page)
      EvKey (KChar 's') [MCtrl] -> do
          liftIO $ makeReport conn id board (unlines $ getEditContents ed) ip
          page <- liftIO $ viewBoard conn board
          continue (AppState conn ip cfg page)
      _                         -> do
          ed' <- handleEvent ev ed
          continue (AppState conn ip cfg (MakeReport board id ed'))
            
-- This dictates where the cursor goes. Note that we don't give a shit
-- unless we're making a post.
appCursor :: AppState -> [CursorLocation] -> Maybe CursorLocation
appCursor (AppState _ _ _ (MakePost _ ui)) =
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

-- Get the IP address of who's connected.
getIP :: String -> IO String
getIP usr = init <$> readCreateProcess (shell command) ""
  where command = "who --ips | grep " ++ usr ++ 
                  " | sort -rk 4 | head -1 | awk '{ print $5 }'"

main :: IO ()
main = do
    cfg <- readConfig <$> readFile "chan.cfg"
    case cfg of
      Left err  -> putStrLn $ "Error parsing config file " ++ err
      Right cfg -> do 
          conn <- open "chan.db"
          ip   <- getIP (chanUser cfg)
          h    <- homepage conn cfg
          ip `seq` defaultMain (makeApp cfg) 
                               (AppState conn ip cfg h)
          return ()

