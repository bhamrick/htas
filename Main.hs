module Main where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Foldable
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Foreign
import Foreign.C.Types
import System.IO
import Text.Printf

import HTas.Direct
import HTas.Low

import Red.Battle
import Red.Intro
import Red.Overworld
import Red.Save
import Search
import MoonManip

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    gb <- create
    loadRomFile gb "pokered.gbc"
    dat <- BS.readFile "pokered_fbee.sav"

    inputRef <- newIORef mempty

    for_ [0..59] $ \frame -> do
        loadSaveData gb (setSaveFrames frame dat)
        (loc, encData) <- fbeeNidoManip gb inputRef
        printf "IGT0: %2d\t%s\t" frame (show loc)
        case encData of
            Nothing -> do
                printf "No encounter\n"
            Just (species, level, dv1, dv2) -> do
                printf "Species: %d\tLevel: %d\tDVs: %02x%02x\n" species level dv1 dv2

fbeeNidoManip :: GB -> IORef Input -> IO (Location, Maybe (Word8, Word8, Word8, Word8))
fbeeNidoManip gb inputRef = do
    reset gb
    doOptimalIntro gb
    setInputGetter gb (readIORef inputRef)
    clearTraceCallback gb

    bufferedWalk gb inputRef . rleExpand $
        [ (i_Left, 8)
        , (i_Right, 3)
        , (i_Left, 7)
        , (i_Down, 3)
        , (i_Left, 2)
        , (i_Up, 4)
        ] <> cycle [(i_Down, 3), (i_Up, 3)]
    loc <- getLocation gb
    encData <- getEncounterData gb
    pure (loc, Just encData)

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''

getEncounterData :: GB -> IO (Word8, Word8, Word8, Word8)
getEncounterData gb = do
    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
    species <- cpuRead gb wEnemyMonSpecies
    level <- cpuRead gb wEnemyMonLevel
    dv1 <- cpuRead gb wEnemyMonAtkDefDV
    dv2 <- cpuRead gb wEnemyMonSpdSpcDV
    pure $ (species, level, dv1, dv2)

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
