{-# LANGUAGE OverloadedStrings #-}
module Wrap (markupWrapping) where

-- This module provides wrapping versions of markup.
-- It's horrifyingly ugly, I'll clean it up sometime.

import Debug.Trace
import Data.Ord
import Data.Char
import Data.List
import Brick.Util
import Data.Array
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
wrap i n (x:xs)
    | x == '\n' = ('\n':) <$> wrap n n xs
    | i == 0    = (['\n',x]++) <$> wrap n n xs
    | otherwise = (x:) <$> wrap (i - 1) n xs

wrapMarkup :: (Eq a, GetAttr a) => Int -> Int -> [[(Text, a)]] -> [(Text, a)] -> [[(Text, a)]]
wrapMarkup i n tmp []     = reverse (map reverse tmp)
wrapMarkup i n tmp (x:xs) =
    let (r, w) = wrap i n $ T.unpack (fst x)
    in if '\n' `notElem` w
         then wrapMarkup r n ((T.pack w, snd x) `appendHead` tmp) xs
         else wrapMarkup r n (reverse (map (\a -> [(T.pack a, snd x)]) (lines w)) ++ tmp) xs
  where appendHead x (xs:xss) = (x:xs):xss

markupWrapping :: (Eq a, GetAttr a) => Markup a -> Widget
markupWrapping m =
    Widget Fixed Fixed $ do
      c <- getContext
      let w     = availWidth c
          pairs = wrapMarkup w w [[]] . markupToList $ m
      imgs <- mapM (fmap horizCat . mapM imgStr) pairs
      return $ def & imageL .~ vertCat imgs
  where imgStr (s, aSrc) = do
          a <- getAttr aSrc
          return $ if T.null s then text' a " " else text' a s

