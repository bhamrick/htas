module Main where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Monoid
import Foreign
import Foreign.C.Types
import Text.Printf

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save

main :: IO ()
main = do
    gb <- create
    loadRomFile gb "pokered.gbc"
    dat <- BS.readFile "pokered.sav"

    for_ [0 .. 59] $ \frame -> do
        (species, level, dv1, dv2) <- fbeeManipFromSave gb (setSaveFrames frame dat)
        printf "IGT0: %2d\tSpecies: %d\tLevel: %d\tDVs: %02x%02x\n" frame species level dv1 dv2

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''

fbeeManipFromSave :: GB -> ByteString -> IO (Word8, Word8, Word8, Word8)
fbeeManipFromSave gb dat = do
    loadSaveData gb dat
    reset gb

    doOptimalIntro gb

    inputRef <- newIORef mempty
    encounterRef <- newIORef False

    setInputGetter gb (readIORef inputRef)
    setTraceCallback gb $ \dat -> do
        let addr = trace_PC dat
        when (addr == 0x7916) $ do
            writeIORef encounterRef True

    bufferedWalk gb inputRef . rleExpand $
        [ (D_Left, 8)
        , (D_Right, 3)
        , (D_Left, 7)
        , (D_Down, 3)
        , (D_Left, 2)
        , (D_Up, 4)
        , (D_Down, 3)
        ] <> cycle [(D_Up, 3), (D_Down, 3)]
    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
    species <- cpuRead gb wEnemyMonSpecies
    level <- cpuRead gb wEnemyMonLevel
    dv1 <- cpuRead gb wEnemyMonAtkDefDV
    dv2 <- cpuRead gb wEnemyMonSpdSpcDV
    pure (species, level, dv1, dv2)

rleExpand :: [(a, Int)] -> [a]
rleExpand runs =
    case runs of
        [] -> []
        (a, 0):rest -> rleExpand rest
        (a, n):rest -> a : rleExpand ((a, n-1):rest)

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
