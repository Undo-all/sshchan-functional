{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Time
import Data.List
import System.IO
import Data.Maybe
import Data.Text (Text)
import Text.Read (readMaybe)
import Database.SQLite.Simple
import qualified Data.Map as M
import qualified Data.Text as T

makeBoard :: Connection -> Text -> Text -> IO ()
makeBoard conn name desc = execute_ conn (Query board)
  where board = T.concat [ "INSERT INTO boards VALUES(NULL,"
                         , T.pack (show name), ","
                         , T.pack (show desc), ")"
                         ]

deleteBoard :: Connection -> Text -> IO ()
deleteBoard conn name = do
    execute_ conn (Query delete)
    execute_ conn "UPDATE SQLITE_SEQUENCE SET SEQ=0 WHERE NAME=boards"
  where delete = T.concat [ "DELETE FROM boards WHERE board_name = "
                          , T.pack (show name)
                          ]

deletePost :: Connection -> Int -> IO ()
deletePost conn id = execute_ conn (Query delete)
  where delete = T.concat [ "DELETE FROM posts WHERE post_id = "
                          , T.pack (show id)
                          ]

banIP :: Connection -> String -> Maybe String -> String -> (Maybe UTCTime) -> IO ()
banIP conn ip board reason time =
    execute conn (Query ban) (ip, board, reason, time)
  where ban = "INSERT INTO boards VALUES(?,?,?,?)"

data Command = Command
             { commandDesc    :: String
             , commandNumArgs :: (Int, Maybe Int)
             , commandFunc    :: Connection -> [String] -> IO ()
             }

correctNumArgs :: Int -> (Int, Maybe Int) -> Bool
correctNumArgs n (x, Nothing) = n >= x
correctNumArgs n (x, Just y)  = n >= x && n <= y

showNumArgs :: (Int, Maybe Int) -> String
showNumArgs (0, Nothing) = "no arguments"
showNumArgs (x, Nothing) = "greater than " ++ show x ++ " arguments"
showNumArgs (x, Just y)
    | x == y    = show x ++ if x == 1 then " argument" else " arguments"
    | otherwise = "between " ++ show x ++ " and " ++ 
                  show y ++ " arguments"

commandHelp :: Command
commandHelp =
    Command
      "list availible commands"
      (0, Just 0)
      (\_ _ -> mapM_ putStrLn $ zipWith showCommand (M.keys commands) (M.elems commands))
  where showCommand name (Command desc numArgs _) =
            name ++ " - " ++ desc ++ ". Takes " ++ showNumArgs numArgs

commandMakeBoard :: Command
commandMakeBoard =
    Command
      "make a board"
      (2, Just 2)
      (\conn [name, desc] -> makeBoard conn (T.pack name) (T.pack desc))

commandDeleteBoard :: Command
commandDeleteBoard =
    Command
      "delete a board"
      (1, Just 1)
      (\conn [name] -> deleteBoard conn (T.pack name))

commandListBoards :: Command
commandListBoards =
    Command
      "list the boards on the sshchan-functional server"
      (0, Nothing)
      list
  where list conn _ = do [xs] <- query_ conn "SELECT board_name FROM boards"
                         putStrLn . intercalate ", " . map T.unpack $ xs

commandDeletePost :: Command
commandDeletePost =
    Command
      "delete a post"
      (1, Just 1)
      delete
  where delete conn [id] = case readMaybe id :: Maybe Int of
                             Just n  -> deletePost conn n
                             Nothing -> putStrLn $ id ++ " is not an integer."

commandDeletePosts :: Command
commandDeletePosts = 
    Command
      "delete all posts passed as arguments"
      (1, Nothing)
      delete
  where delete conn xs = let nums = catMaybes . map readInt $ xs
                         in mapM_ (deletePost conn) nums
        readInt x      = read x :: Maybe Int

commandDeleteByIP :: Command
commandDeleteByIP =
    Command
      "delete all posts by an IP across boards"
      (1, Just 1)
      delete
  where delete conn [ip] = execute_ conn (Query $ queryDelete ip)
        queryDelete ip   =
            T.concat [ "DELETE FROM posts WHERE post_ip = \""
                     , T.pack ip, "\""
                     ]

commandBanIP :: Command
commandBanIP =
    Command
      "ban a user by IP (time format: seconds)"
      (2, Just 4)
      ban
  where ban conn [ip, reason]              = banIP conn ip Nothing reason Nothing
        ban conn [ip, board, reason]       =
            banIP conn ip (parseBoard board) reason Nothing
        ban conn [ip, board, reason, time] = do
            let add = parseTime time
            currTime <- getCurrentTime
            banIP conn ip (parseBoard board) reason (Just $ addUTCTime add currTime)

        parseTime xs     = fromIntegral ((10^12) * readInt xs) 
        parseBoard "all" = Nothing
        parseBoard xs    = Just xs
        readInt x = read x :: Int

commandGetIP :: Command
commandGetIP =
    Command
      "get the IP of the poster of a post"
      (1, Just 1)
      getIP
  where getIP conn [id] = do
            [Only ip] <- query_ conn (Query $ queryIP id)
            putStrLn ip
        queryIP id = T.concat [ "SELECT post_ip FROM posts WHERE post_id = "
                              , T.pack (show id)
                              ]
        

commands :: M.Map String Command
commands = M.fromList
               [ ("help", commandHelp)
               , ("make-board", commandMakeBoard)
               , ("delete-board", commandDeleteBoard)
               , ("list-boards", commandListBoards)
               , ("delete-post", commandDeletePost)
               , ("delete-posts", commandDeletePosts)
               , ("delete-by-ip", commandDeleteByIP)
               , ("ban-ip", commandBanIP)
               , ("get-ip", commandGetIP)
               ]

eval :: Connection -> String -> [String] -> IO ()
eval conn x xs =
    case M.lookup x commands of
      Just (Command _ n f) ->
        if correctNumArgs (length xs) n
          then f conn xs
          else let expected = " (expected " ++ showNumArgs n ++ ", got " ++
                              show (length xs) ++ ")"
               in putStrLn $ 
                      "Wrong number of arguments to command " ++ x ++ expected
      Nothing              -> putStr $ "Command not found: " ++ x

repl :: Connection -> IO ()
repl conn = do
    putStr "> "
    hFlush stdout
    xs <- getLine
    if all isSpace xs
      then repl conn
      else let xs' = args [] [] False xs
           in do eval conn (head xs') (tail xs')
                 repl conn
  where args tmp res _ []
            | null tmp  = reverse res
            | otherwise = reverse (reverse tmp:res)
        args tmp res True ('\\':'"':xs) = args ('"':tmp) res True xs
        args tmp res True ('"':xs)      = args [] (reverse tmp:res) False xs
        args tmp res True (c:xs)        = args (c:tmp) res True xs
        args tmp res False ('"':xs)
            | null tmp  = args [] res True xs
            | otherwise = args [] (reverse tmp:res) True xs
        args tmp res False (c:xs) 
            | isSpace c =
              if null tmp
                then args [] res False xs 
                else args [] (reverse tmp:res) False xs
            | otherwise = args (c:tmp) res False xs

main :: IO ()
main = do
    putStrLn "Welcome to the sshchan-functional admin console."
    putStrLn "At the moment, it encompasses only extremely basic"
    putStrLn "functionality. Type 'help' for a list of commands."
    conn <- open "chan.db"
    repl conn

