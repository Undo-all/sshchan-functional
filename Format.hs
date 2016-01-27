module Format (parseFormat) where

import Data.Char
import Data.List
import Brick.Util
import Brick.Markup
import Data.Text (Text)
import Text.Read (readMaybe)
import Graphics.Vty.Attributes
import qualified Data.Text as T
import qualified Data.Text.Markup as M

data Format = Bold
            | Underline
            | Fg Int
            | Bg Int
            deriving (Eq, Show)

fmtToAttr :: [Format] -> Attr
fmtToAttr = foldl conv defAttr 
  where conv a fmt =
          case fmt of
            Bold      -> a `withStyle` bold
            Underline -> a `withStyle` underline
            Fg n      -> a `withForeColor` (ISOColor $ fromIntegral n)
            Bg n      -> a `withBackColor` (ISOColor $ fromIntegral n)

append :: String -> [Format] -> [(Text, Attr)] -> [(Text, Attr)]
append tmp fmt res
    | null tmp  = res
    | otherwise = (T.pack (reverse tmp), fmtToAttr fmt) : res

parse :: String -> [Format] -> [(Text, Attr)] -> String -> Markup Attr
parse tmp fmt res []          = M.fromList . reverse $ append tmp fmt res
parse tmp fmt res ('\\':c:xs) = parse (c:tmp) fmt res xs

parse tmp fmt res ('*':xs)
    | Bold `elem` fmt = 
      parse [] (filter (not . (==Bold)) fmt) (append tmp fmt res) xs
    | otherwise       = parse [] (Bold:fmt) (append tmp fmt res) xs

parse tmp fmt res ('_':xs)
    | Underline `elem` fmt = 
      parse [] (filter (not . (==Underline)) fmt) (append tmp fmt res) xs
    | otherwise       = parse [] (Underline:fmt) (append tmp fmt res) xs

parse tmp fmt res ('[':xs) =
    case findIndex (==';') xs of
      Just x  -> case findIndex (==':') xs of
                   Just y  -> case (,) <$> parseInt (take x xs) <*> parseInt (drop (x+1) . take y $ xs) of
                                Nothing     -> parse ('[':tmp) fmt res xs
                                Just (a, b) ->
                                  parse "" (Fg (clamp 0 a 15):Bg (clamp 0 b 15):fmt) (append tmp fmt res) (drop (y+1) xs)
                   Nothing -> parse ('[':tmp) fmt res xs
      Nothing -> case findIndex (==':') xs of
                   Just x  -> case parseInt (take x xs) of
                                Nothing -> parse ('[':tmp) fmt res xs
                                Just a  ->
                                  parse "" (Fg (clamp 0 a 15):fmt) (append tmp fmt res) (drop (x+1) xs)
                   Nothing -> parse ('[':tmp) fmt res xs
  where parseInt x = readMaybe x :: Maybe Int

parse tmp fmt res (']':xs) = 
    case find isColor fmt of
      Nothing -> parse (']':xs) fmt res xs
      Just _  -> parse [] (filter (not . isColor) fmt) (append tmp fmt res) xs
  where isColor (Fg _) = True
        isColor (Bg _) = True
        isColor _      = False

parse tmp fmt res (c:xs)      = parse (c:tmp) fmt res xs

parseFormat :: String -> Markup Attr
parseFormat = parse "" [] []

