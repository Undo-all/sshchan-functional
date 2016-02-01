{-# LANGUAGE OverloadedStrings #-}
module Util
( genTripcode
, makePost
, makeReport
, getBoardName
, getBoardID
, getReplies
, getThread
, getThreads
, isThreadLocked
) where

import Types
import Format
import Data.Char
import Data.Maybe
import Data.Text (Text)
import System.Unix.Crypt
import Data.Vector (Vector)
import Database.SQLite.Simple
import qualified Data.Text as T
import qualified Data.Vector as V

-- Tripcode generation code adapted 
-- from http://cairnarvon.rotahall.org/2009/01/09/ofioc/

-- Generate salt from password.
salt :: String -> String
salt = map f . take 2 . tail . (++"H..")
  where f c | c `notElem` ['.'..'z'] = '.'
            | c `elem` [':'..'@']    = chr $ ord c + 7
            | c `elem` ['['..'`']    = chr $ ord c + 6
            | otherwise              = c

-- The actual tripcode generation.
tripcode :: Text -> IO Text
tripcode xs = let last10 xs = drop (length xs - 10) xs
                  pass      = T.unpack xs
              in T.pack . last10 <$> crypt pass (salt pass)

-- Takes a name, and returns a tripcode, but only if the name asks for
-- a tripcode specifically (name#password). Otherwise, it just returns the
-- name.
genTripcode :: Text -> IO (Maybe Text)
genTripcode xs 
    | isJust (T.find (=='#') xs) = do
        let (name, pass) = fmap T.tail . T.breakOn "#" $ xs
        trip <- tripcode pass
        return $ Just (T.concat [name, " !",  trip])
    | otherwise                  = return (Just xs)

-- Make a post (ofc)
makePost :: Connection -> IP -> Maybe Text -> Maybe Text -> Text -> Int -> Maybe Int -> IO ()
makePost conn ip subject name content board reply = do
    trip <- maybe (return Nothing) genTripcode name
    execute conn post (ip, subject, trip, content, board, reply)
    case reply of
      Just n  -> execute conn bump (Only n)
      Nothing -> return ()
  where post = "INSERT INTO posts \
               \VALUES(NULL,?,date('now'),datetime('now'),0,0,?,?,?,?,?)"
        bump = "UPDATE posts SET \
               \post_last_bumped = datetime('now') WHERE post_id = ?"

-- Make a report.
makeReport :: Connection -> Int -> Int -> String -> IP -> IO ()
makeReport conn id board reason ip = 
    execute conn report (id, board, reason, ip)
  where report = "INSERT INTO reports VALUES(NULL, ?, ?, ?, datetime('now'), ?)"

-- Gets the name of a board from it's ID.
getBoardName :: Connection -> Int -> IO String
getBoardName conn id = do
    [Only name] <- query conn queryName (Only id)
    return name
  where queryName = "SELECT board_name FROM boards WHERE board_id = ?"

-- Opposite of getBoardName: gets a board's ID from it's name.
getBoardID :: Connection -> String -> IO Int
getBoardID conn name = do
    [Only id] <- query conn queryID (Only name)
    return id
  where queryID = "SELECT board_id FROM boards WHERE board_name = ?"

-- Fetch the replies to a post (slow?)
getReplies :: Connection -> Bool -> Int -> Int -> IO [Post]
getReplies conn limited id board = do
    xs <- query conn (Query queryReplies) (id, board)
    return $ if limited then reverse xs else xs
  where base = "SELECT post_id, post_date, post_by, \
                \post_subject, post_content FROM posts WHERE \
                \post_reply = ? \
                \AND post_board = ? \
                \ORDER BY post_id"
        queryReplies
            | limited = T.concat [ base, " DESC LIMIT 5" ]
            | otherwise = T.concat [ base, " ASC" ]

-- Get a thread from it's ID.
getThread :: Connection -> Bool -> Int -> Int -> IO Thread
getThread conn limited board id = do
    [(id, date, by, subj, cont, stick, lock)] <- query conn queryOp (id, board)
    let post = Post subj by date id (parseFormat cont)
    replies <- V.fromList <$> getReplies conn limited id board 
    if limited
      then do [Only n] <- query conn queryLen (id, board)
              if n > 5
                then return (Thread post replies (Just (n-5)) stick lock)
                else return (Thread post replies Nothing stick lock)
      else return (Thread post replies Nothing stick lock)
  where queryOp = "SELECT post_id, post_date, post_by, post_subject, \
                  \post_content, post_stickied, post_locked FROM posts \
                  \WHERE post_id = ? AND post_board = ?"
        queryLen = "SELECT count(*) FROM posts WHERE post_reply = ? \
                   \AND post_board = ?"

-- Get all the threads from a board
getThreads :: Connection -> Int -> IO (Vector Thread)
getThreads conn board = do
    ops <- V.fromList <$> query conn queryOps (Only board) 
    V.mapM (getThread conn True board . (\(Only id) -> id)) ops
  where queryOps = "SELECT post_id FROM posts WHERE post_reply IS NULL \
                   \AND post_board = ? ORDER BY post_stickied, \
                   \post_last_bumped DESC"

-- Tests if a thread is locked.
isThreadLocked :: Connection -> Int -> Int -> IO Bool
isThreadLocked conn board id = do
    [Only b] <- query conn queryLocked (id, board)
    return b
  where queryLocked = "SELECT post_locked FROM posts WHERE \
                      \post_id = ? AND post_board = ?" 

