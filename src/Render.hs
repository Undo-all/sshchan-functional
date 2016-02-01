module Render where

import Page
import Wrap
import Brick
import Types
import Data.Time
import Graphics.Vty
import Brick.Widgets.Edit
import Data.Vector (Vector)
import Brick.Widgets.Border
import Graphics.Vty.Attributes
import qualified Data.Text as T
import Brick.Widgets.Border.Style
import qualified Data.Vector as V

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
                         padLeft (Pad 2) (vBox . V.toList . V.map (renderPost False False False) $ xs)
    | selected == 0    = visible $ renderPost True stickied locked op <=> omitMsg <=>
                         padLeft (Pad 2) (vBox . V.toList . V.map (renderPost False False False) $ xs)
    | otherwise        = renderPost False stickied locked op <=> omitMsg <=>
                         padLeft (Pad 2) 
                             (vBox . V.toList . V.imap renderSelected $ xs)
  where omitMsg =
            case omitted of
              Nothing -> emptyWidget
              Just n  -> padLeft (Pad 2) . str $ 
                             show n ++ " posts omitted. Hit enter to view."
        renderSelected index post
            | index + 1 == selected = visible (renderPost True False False post)
            | otherwise             = renderPost False False False post

-- Render several threads (these comments are helpful, aren't they?)
renderThreads :: Int -> Vector Thread -> Widget
renderThreads selected = vBox . V.toList . V.imap renderSelected 
  where renderSelected index thread
            | index == selected = renderThread 0 thread
            | otherwise         = renderThread (-1) thread


-- Render a PostUI.
renderPostUI :: PostUI -> Widget
renderPostUI (PostUI _ ed1 ed2 ed3 ed4) =
    vBox [ info, fields, content ]
  where save    = padRight Max . padBottom (Pad 1) $ str "Ctrl+S to save"
        cancel  = padBottom (Pad 1) $ str "Ctrl+Z to go back"
        info    = hLimit 100 (save <+> cancel)
        editors = [ str "Subject: ", renderEditor ed1
                  , padLeft (Pad 1) $ str "Name: ", renderEditor ed2
                  , padLeft (Pad 1) $ str "Reply to: ", renderEditor ed3
                  ] 
        fields  = vLimit 25 . hLimit 100 . padBottom (Pad 1) . hBox $ editors
        content = vLimit 25 . hLimit 100 $ renderEditor ed4

