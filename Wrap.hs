{-# LANGUAGE OverloadedStrings #-}
module Wrap (markupWrapping) where

-- This module provides wrapping versions of markup.
-- It's horrifyingly ugly, I'll clean it up sometime.

import Data.Char
import Data.List
import Brick.Util
import Brick.Types
import Brick.Markup
import Graphics.Vty
import Control.Lens
import Data.Default
import Control.Monad
import Control.DeepSeq
import Data.Text (Text)
import Data.Text.Markup
import Brick.Widgets.Core
import qualified Data.Text as T

wrap :: Int -> Int -> String -> (Int, String)
wrap i n [] = (i, "")
wrap i n xs
    | isSpace (head xs)    = (head xs :) <$> wrap (i-1) n (tail xs)
    | length word > n      = (("\n"++(take n word)++"\n")++) <$> wrap (length word) n rest
    | i - length word <= 0 = (('\n':word)++) <$> wrap (n - length word) n rest
    | otherwise            = (word++) <$> wrap (i - length word) n rest
  where word = takeWhile (not . isSpace) xs
        rest = dropWhile (not . isSpace) xs

wrapMarkup :: (Eq a, GetAttr a) => Int -> Int -> [(Text, a)] -> [([Text], a)]
wrapMarkup i n [] = []
wrapMarkup i n (x:xs) =
    let (r, w) = wrap i n $ T.unpack (fst x)
    in (map T.pack (getLines w), snd x) : wrapMarkup i n xs

getLines :: String -> [String]
getLines s =
    let theLines     = map (fixEnd . fixEmpty) $ lines s
        fixEmpty []  = " " :: String
        fixEmpty l   = l
        fixEnd       = reverse . dropWhile isSpace . reverse
    in force theLines

markupWrapping :: (Eq a, GetAttr a) => Markup a -> Widget
markupWrapping m =
    Widget Fixed Fixed $ do
      c <- getContext
      let w     = availWidth c
          pairs = wrapMarkup w w . markupToList $ m
      imgs <- forM pairs $ \(l, aSrc) -> do
          a <- getAttr aSrc
          case l of
            []       -> return emptyImage
            [one]    -> return $ text' a one
            multiple ->
                let maxLength    = maximum $ T.length <$> multiple
                    lineImgs     = lineImg <$> multiple
                    lineImg lStr = text' a (T.concat [lStr, T.replicate (maxLength - T.length lStr) " "])
                in return $ vertCat lineImgs
      return $ def & imageL .~ horizCat imgs

