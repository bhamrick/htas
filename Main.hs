module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Foreign
import Foreign.C.Types
import Text.Printf

import HTas.Direct
import HTas.Low

main :: IO ()
main = do
    gb <- create
    loadRomFile gb "pokered.gbc"
    isCgb gb >>= print

    setExecCallback gb (\addr -> do
        when (addr == 0x7A97) $ do
            getMemoryArea gb HRAM >>= dumpData
            getRegs gb >>= printRegs
            getCycleCount gb >>= \cyc -> printf "cycle = %d ; divider = %x\n" cyc (cyc `div` 2)
            cpuRead gb 0xFF04 >>= printf "rDiv = %02x\n"
            putStrLn ""
        )

    replicateM_ 500 $ do
        advanceFrame gb

dumpData :: BS.ByteString -> IO ()
dumpData = go . BS.unpack
    where
    go :: [Word8] -> IO ()
    go ws = case ws of
        [] -> pure ()
        _ -> do
            let (b:bs, rest) = splitAt 16 ws
            printf "%02x" b
            traverse_ (printf " %02x") bs
            printf "\n"
            go rest
