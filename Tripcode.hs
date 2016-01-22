{-# LANGUAGE OverloadedStrings #-}

-- Why is this in it's own module, you may ask? Well, you see, the code
-- residing here is so increadably ugly and namespace-destroying, that
-- I couldn't possibily let it touch SSHChan.hs for moral reasons.

module Tripcode (genTripcode) where

import Debug.Trace
import Codec.Text.IConv 
import System.Unix.Crypt
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy.Encoding as TL
import Data.ByteString.Internal (w2c, c2w)

utf8toShiftJIS :: TL.Text -> BS.ByteString
utf8toShiftJIS = convertFuzzy Discard "UTF-8" "SHIFT_JISx0213" . TL.encodeUtf8

shiftJIStoUtf8 :: BS.ByteString -> TL.Text
shiftJIStoUtf8 = TL.decodeUtf8 . convertFuzzy Discard "SHIFT_JISx0213" "UTF-8"

-- generate a tripcode and append it to the name
genTripcode :: Maybe T.Text -> IO (Maybe T.Text)
genTripcode Nothing = return Nothing
genTripcode (Just xs)
    | isJust (T.find (=='#') xs) = do
        let (name, pass) = T.breakOn "#" xs
            salt         = (\x -> trace (show x) x) . finalize . BS.take 2 . BS.tail . (`BS.append` "H..") $ 
                               (utf8toShiftJIS . TL.pack . T.unpack $ T.tail pass)
        trip <- last10 <$> crypt (tail $ T.unpack pass) (TL.unpack . shiftJIStoUtf8 $ salt)
        return . Just $ T.concat [name, " !", T.pack trip]
    | otherwise                  = return . Just $ xs
  where last10 xs = drop (length xs - 10) xs
        peroidize = BS.map (\c -> if c `elem` [0x4E..0x7A] then c else 0x2E)
        letterize = BS.map replace
        finalize  = letterize . peroidize
        replace c =
            case c of
              0x3A -> 0x41
              0x3B -> 0x42
              0x3C -> 0x43
              0x3D -> 0x44
              0x3E -> 0x45
              0x3F -> 0x46
              0x40 -> 0x47
              0x5B -> 0x61
              0x5C -> 0x62
              0x5D -> 0x63
              0x5E -> 0x64
              0x5F -> 0x65
              0x60 -> 0x66
              _    -> c
               

