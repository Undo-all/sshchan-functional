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
  where ban = "INSERT INTO bans VALUES(?,?,?,?)"

unbanIP :: Connection -> String -> IO ()
unbanIP conn ip = execute_ conn (Query unban) 
  where unban = T.concat [ "DELETE FROM bans WHERE ban_ip = ", T.pack (show ip)]

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
      "list availible commands or get description of specific command"
      (0, Just 1)
      help
  where help _ []                                 =
            mapM_ putStrLn $ zipWith showCommand (M.keys commands) (M.elems commands)
        help _ [name]                             =
            case M.lookup name commands of
              Just (Command desc _ _) -> putStrLn desc
              Nothing                 -> putStrLn $ "Command not found: " ++ name
        showCommand name (Command desc numArgs _) =
            name ++ ": " ++ desc ++ ". Takes " ++ showNumArgs numArgs

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

commandDeletePosts :: Command
commandDeletePosts = 
    Command
      "delete every post passed as an argument"
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
      "ban a user by IP (time format: minutes)"
      (2, Just 4)
      ban
  where ban conn [ip, reason]              = banIP conn ip Nothing reason Nothing
        ban conn [ip, board, reason]       =
            banIP conn ip (parseBoard board) reason Nothing
        ban conn [ip, board, reason, time] = do
            let add = parseTime time
            currTime <- getCurrentTime
            banIP conn ip (parseBoard board) reason (Just $ addUTCTime add currTime)

        parseTime xs     = fromIntegral (60 * readInt xs) 
        parseBoard "all" = Nothing
        parseBoard xs    = Just xs
        readInt x = read x :: Int

commandUnbanIP :: Command
commandUnbanIP =
    Command
      "unban a user by IP"
      (1, Just 1)
      unban
  where unban conn [ip] = unbanIP conn ip 

commandGetIP :: Command
commandGetIP =
    Command
      "get the IP of the poster of a post"
      (1, Just 1)
      getIP
  where getIP conn [id] = do
            [Only ip] <- query conn queryIP (Only id)
            putStrLn ip
        queryIP = "SELECT post_ip FROM posts WHERE post_id = ?"

commandListBans :: Command
commandListBans =
    Command
      "list the currently active bans"
      (0, Just 0)
      list
  where list conn _ = do
            bans <- query_ conn "SELECT * FROM bans"
            putStrLn "IP\t\t\tboard\t\t\treason\t\t\tuntil"
            mapM_ (printBan conn) bans
        printBan :: Connection -> (String, Maybe Int, String, Maybe UTCTime) -> IO ()
        printBan conn (ip, board, reason, until) = do
            [Only name] <- case board of
                             Nothing -> return [Only "all boards"]
                             Just b  -> query conn queryName (Only b)
            putStrLn $ ip ++ "\t\t" ++ name ++ "\t\t" ++ 
                       reason ++ "\t\t" ++ maybe "forever" show until
        queryName = "SELECT board_name FROM boards WHERE board_id = ?"

commandClearBans :: Command
commandClearBans =
    Command
      "clear all bans"
      (0, Just 0)
      (\conn _ -> execute_ conn "DELETE FROM boards")

commandViewReports :: Command
commandViewReports =
    Command
      "view reports"
      (0, Just 0)
      view
  where view conn _ = do
            reports <- query_ conn "SELECT * FROM reports"
            putStrLn "ID\t\tpost\t\tboard\t\treason\t\t\ttime\t\t\t\tby"
            mapM_ (printReport conn) reports
        printReport :: Connection -> (Int, Int, Int, String, UTCTime, String) -> IO ()
        printReport conn (id, post, board, reason, time, by) = do
            [Only name] <- query conn "SELECT board_name FROM boards WHERE board_id = ?" (Only id)
            putStrLn $ show id ++ "\t\t" ++ show post ++ "\t\t" ++ name ++ "\t" ++
                       show reason ++ "\t\t" ++ show time ++ "\t\t" ++ by

commandDismissReports :: Command
commandDismissReports =
    Command
      "dismiss every report passed as an argument (by ID)"
      (1, Nothing)
      dismissReports
  where dismissReports conn xs = let nums = catMaybes . map readInt $ xs
                                 in mapM_ (dismiss conn) nums
        dismiss conn id        =
            execute conn "DELETE FROM reports WHERE report_id = ?" (Only id)
        readInt x              = readMaybe x :: Maybe Int

commandClearReports :: Command
commandClearReports =
    Command
      "clear all reports"
      (0, Nothing)
      (\conn _ -> execute_ conn "DELETE FROM reports")

commands :: M.Map String Command
commands = M.fromList
               [ ("help", commandHelp)
               , ("make-board", commandMakeBoard)
               , ("delete-board", commandDeleteBoard)
               , ("list-boards", commandListBoards)
               , ("delete-posts", commandDeletePosts)
               , ("delete-by-ip", commandDeleteByIP)
               , ("ban-ip", commandBanIP)
               , ("unban-ip", commandUnbanIP)
               , ("list-bans", commandListBans)
               , ("clear-bans", commandClearBans)
               , ("view-reports", commandViewReports)
               , ("dismiss-reports", commandDismissReports)
               , ("clear-reports", commandClearReports)
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
      Nothing              -> putStrLn $ "Command not found: " ++ x

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

