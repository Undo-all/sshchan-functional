{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.List
import System.IO
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

commandDeletePost :: Command
commandDeletePost =
    Command
      "delete a post"
      (1, Just 1)
      delete
  where delete conn [id] = case readMaybe id :: Maybe Int of
                             Just n  -> deletePost conn n
                             Nothing -> putStrLn $ id ++ " is not an integer."

commandListBoards :: Command
commandListBoards =
    Command
      "list the boards on the sshchan-functional server"
      (0, Nothing)
      list
  where list conn _ = do [xs] <- query_ conn "SELECT board_name FROM boards"
                         putStrLn . intercalate ", " . map T.unpack $ xs

commands :: M.Map String Command
commands = M.fromList
               [ ("help", commandHelp)
               , ("make-board", commandMakeBoard)
               , ("delete-board", commandDeleteBoard)
               , ("delete-post", commandDeletePost)
               , ("list-boards", commandListBoards)
               ]

eval :: Connection -> String -> [String] -> IO ()
eval conn x xs =
    case M.lookup x commands of
      Just (Command _ n f) ->
        if correctNumArgs (length xs) n
          then f conn xs
          else let expected = " (expected " ++ showNumArgs n ++ ", got " ++ 
                              show (length xs) ++ ")"
               in putStr $ "Wrong number of arguments to command " ++ x ++ expected
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

