module Red.Save where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

savePositionMap = 0x260A :: Int
savePositionY = 0x260D :: Int
savePositionX = 0x260E :: Int

saveTimeHours = 0x2CED :: Int
saveTimeMaxed = 0x2CEE :: Int
saveTimeMinutes = 0x2CEF :: Int
saveTimeSeconds = 0x2CF0 :: Int
saveTimeFrames = 0x2CF1 :: Int

saveMainDataStart = 0x2598 :: Int
saveMainDataEnd = 0x3523 :: Int
saveMainDataChecksum = 0x3523 :: Int

editByte :: Int -> Word8 -> ByteString -> ByteString
editByte idx b dat =
    let (start, rest) = BS.splitAt idx dat
    in case BS.uncons rest of
        Nothing -> start
        Just (_, end) -> BS.concat [start, BS.singleton b, end]

computeChecksum :: Int -> Int -> ByteString -> Word8
computeChecksum start end dat =
    let region = BS.take (end-start) . BS.drop start $ dat
    in
    complement (BS.foldl' (+) 0 region)
