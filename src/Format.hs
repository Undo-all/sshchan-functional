module Format (parseFormat) where

import Data.Char
import Data.List
import Brick.Util
import Data.Monoid
import Brick.Markup
import Data.Text (Text)
import Control.Applicative
import Text.Read (readMaybe)
import Data.Text.Lazy.Builder
import Graphics.Vty.Attributes
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Markup as M

data Format = Bold
            | Underline
            | Fg Int
            | Bg Int
            deriving Eq

parseFormat :: Text -> Markup Attr
parseFormat = parse mempty [] []

fmtToAttr :: [Format] -> Attr
fmtToAttr = foldl conv defAttr
  where conv a fmt =
          case fmt of
            Bold      -> a `withStyle` bold
            Underline -> a `withStyle` underline
            Fg n      -> a `withForeColor` (ISOColor $ fromIntegral n)
            Bg n      -> a `withBackColor` (ISOColor $ fromIntegral n)

append :: Builder -> [Format] -> [(Text, Attr)] -> [(Text, Attr)]
append tmp fmt res =
    let xs = toStrict $ toLazyText tmp
    in if T.null xs then res else (xs, fmtToAttr fmt) : res

-- ...let's just pretend this is autogenerated, or something.
parse :: Builder -> [Format] -> [(Text, Attr)] -> Text -> Markup Attr
parse tmp fmt res txt
    | T.null txt         = M.fromList . reverse $ append tmp fmt res
    | T.head txt == '\\' =
      if T.length txt >= 2
        then parse (tmp <> singleton (T.head (T.tail txt))) 
                   fmt res (T.drop 2 txt)
        else parse (tmp <> singleton '\\')
                   fmt res (T.tail txt)
    | T.head txt == '*'  =
      if Bold `elem` fmt
        then parse mempty (filter (/=Bold) fmt) 
                   (append tmp fmt res) (T.tail txt)
        else parse mempty (Bold:fmt) (append tmp fmt res) (T.tail txt)
    | T.head txt == '_'  =
      if Underline `elem` fmt
        then parse mempty (filter (/=Underline) fmt) 
             (append tmp fmt res) (T.tail txt)
        else parse mempty (Underline:fmt) (append tmp fmt res) (T.tail txt)
    | T.head txt == '[' = let txt' = (T.tail txt) in
      case T.findIndex (==';') txt' of
        Just x ->
          case T.findIndex (==':') txt' of
            Just y ->
              case (,) <$> parseInt (T.take x txt') <*> 
                           parseInt (T.drop (x+1) . T.take y $ txt') of
                Nothing     -> parse (tmp <> singleton '[') fmt res txt'
                Just (a, b) ->
                  parse mempty (Fg (clamp 0 a 15):Bg (clamp 0 b 15):fmt)
                        (append tmp fmt res) (T.drop (y+1) txt')
            Nothing -> parse (singleton '[' <> tmp) fmt res txt'
        Nothing -> case T.findIndex (==':') txt' of
                     Just x ->
                       case parseInt (T.take x txt') of
                         Just a  ->
                           parse mempty 
                                 (Fg (clamp 0 a 15):fmt) (append tmp fmt res) 
                                 (T.drop (x+1) txt')
                         Nothing -> parse (tmp <> singleton '[') fmt res txt'
                     Nothing -> parse (tmp <> singleton '[') fmt res txt' 
    | T.head txt == ']' = 
        case find isColor fmt of
          Nothing -> parse (tmp <> singleton ']') fmt res (T.tail txt)
          Just _  ->
            parse mempty (filter (not . isColor) fmt) 
                  (append tmp fmt res) (T.tail txt)
    | otherwise         =
      parse (tmp <> singleton (T.head txt)) fmt res (T.tail txt)
  where parseInt x     = readMaybe (T.unpack x) :: Maybe Int
        isColor (Fg _) = True
        isColor (Bg _) = True
        isColor _      = False

{-
parse tmp fmt res []          = M.fromList . reverse $ append tmp fmt res
parse tmp fmt res ('\\':c:xs) = parse (c:tmp) fmt res xs

parse tmp fmt res ('*':xs)
    | Bold `elem` fmt = 
      parse [] (filter (/=Bold) fmt) (append tmp fmt res) xs
    | otherwise       = parse [] (Bold:fmt) (append tmp fmt res) xs

parse tmp fmt res ('_':xs)
    | Underline `elem` fmt = 
      parse [] (filter (/=Underline) fmt) (append tmp fmt res) xs
    | otherwise       = parse [] (Underline:fmt) (append tmp fmt res) xs

-- Parsing can be ugly.
parse tmp fmt res ('[':xs) =
    case elemIndex ';' xs of
      Just x  -> case elemIndex ':' xs of
                   Just y  -> case (,) <$> parseInt (take x xs) <*> parseInt (drop (x+1) . take y $ xs) of
                                Nothing     -> parse ('[':tmp) fmt res xs
                                Just (a, b) ->
                                  parse "" (Fg (clamp 0 a 15):Bg (clamp 0 b 15):fmt) (append tmp fmt res) (drop (y+1) xs)
                   Nothing -> parse ('[':tmp) fmt res xs
      Nothing -> case elemIndex ':' xs of
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
-}
