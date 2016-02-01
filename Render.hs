module Render where

import Wrap
import Brick
import Types
import Data.Time
import Graphics.Vty
import Brick.Widgets.Border
import Graphics.Vty.Attributes
import qualified Data.Text as T
import Brick.Widgets.Border.Style

-- Renders a post.
renderPost :: Bool -> Bool -> Bool -> Post -> Widget
renderPost selected stickied locked (Post subject name date id content) =
    let renderSubject =
          raw . string (Attr (SetTo bold) (SetTo blue) Default) . T.unpack
        subjectInfo   = maybe emptyWidget renderSubject subject
        renderName    = raw . string (Attr (SetTo bold) (SetTo green) Default)
        nameInfo      = renderName $ maybe "Anonymous" T.unpack name
        dateInfo      = str (showGregorian date)
        idInfo        = str ("No. " ++ show id)
        stickmsg      = if stickied then str "(stickied)" else emptyWidget
        lockmsg       = if locked then str "(locked)" else emptyWidget
        allInfo       = [subjectInfo, nameInfo, dateInfo, idInfo, stickmsg, lockmsg]
        info          = hBox $ map (padRight (Pad 1)) allInfo
        body          = markupWrapping content
        borderStyle   = if selected then unicodeBold else unicode
    in withBorderStyle borderStyle . border $
           padBottom (Pad 1) info <=> padRight (Pad 1) body

-- Render a thread 
renderThread :: Int -> Thread -> Widget
renderThread selected (Thread op xs omitted stickied locked)
    | selected == (-1) = renderPost False stickied locked op <=> omitMsg <=>
                         padLeft (Pad 2) (vBox . map (renderPost False False False) $ xs)
    | selected == 0    = visible $ renderPost True stickied locked op <=> omitMsg <=>
                         padLeft (Pad 2) (vBox . map (renderPost False False False) $ xs)
    | otherwise        = renderPost False stickied locked op <=> omitMsg <=>
                         padLeft (Pad 2) 
                             (vBox . map renderSelected . indexes $ xs)
  where indexes xs = zip xs [0..length xs - 1]
        omitMsg    =
            case omitted of
              Nothing -> emptyWidget
              Just n  -> padLeft (Pad 2) . str $ 
                             show n ++ " posts omitted. Hit enter to view."
        renderSelected (post, index)
            | index + 1 == selected = visible (renderPost True False False post)
            | otherwise             = renderPost False False False post

-- Render several threads (these comments are helpful, aren't they?)
renderThreads :: Int -> [Thread] -> Widget
renderThreads selected = vBox . map renderSelected . indexes
  where indexes xs = zip xs [0..length xs - 1]
        renderSelected (thread, index)
            | index == selected = renderThread 0 thread
            | otherwise         = renderThread (-1) thread

