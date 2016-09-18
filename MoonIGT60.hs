module Main where

import Control.Concurrent
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

-- Copy output from moonsearch here
segmentPaths = [[Input 16,Input 128,Input 16,Input 16,Input 16,Input 16,Input 16],[Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 16],[Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 32,Input 64,Input 64,Input 64,Input 32,Input 64,Input 32,Input 64,Input 32,Input 64,Input 64,Input 64,Input 64,Input 64,Input 32,Input 32,Input 32],[Input 128,Input 32,Input 32,Input 32,Input 32,Input 128,Input 32,Input 32,Input 128,Input 32,Input 128,Input 32,Input 32,Input 32,Input 32,Input 32,Input 128,Input 128,Input 128,Input 128],[Input 32,Input 32,Input 32,Input 32,Input 32,Input 128,Input 32,Input 32,Input 128,Input 32],[Input 16,Input 16,Input 16,Input 64],[Input 128,Input 128,Input 128,Input 32,Input 32,Input 32,Input 128],[Input 16,Input 16,Input 16,Input 16,Input 64,Input 16,Input 16,Input 16,Input 64,Input 16],[Input 128,Input 32,Input 128,Input 128,Input 128,Input 128,Input 128],[Input 64,Input 64,Input 64,Input 32,Input 64,Input 32,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 32,Input 64,Input 32,Input 32,Input 64,Input 32,Input 64,Input 32],[Input 128,Input 128,Input 128,Input 16,Input 16],[Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 16,Input 16,Input 128,Input 128,Input 128,Input 16,Input 16,Input 16,Input 16,Input 16,Input 16,Input 16,Input 16,Input 16,Input 16,Input 128,Input 16,Input 16,Input 16,Input 16],[Input 16,Input 64,Input 16,Input 64,Input 64,Input 16,Input 16,Input 16],[Input 16,Input 128,Input 128,Input 16,Input 16,Input 16,Input 16,Input 16],[Input 64,Input 64,Input 16,Input 16,Input 16,Input 16],[Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 32,Input 32,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 128,Input 32,Input 32,Input 128,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32,Input 32],[Input 64,Input 64,Input 32,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64,Input 64]]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    gb <- create
    inputRef <- newIORef mempty
    loadRomFile gb "pokered.gbc"
    baseSave <- BS.readFile "pokered_r3_lass.sav"

    for_ [0..59] $ \frame -> do
        loadSaveData gb (setSaveFrames frame baseSave)
        reset gb
        doOptimalIntro gb
        setInputGetter gb (readIORef inputRef)
        clearTraceCallback gb
        (loc, enc) <- runPath gb inputRef segmentPaths
        printf "Frame %2d\t%s\t" frame (show loc)
        case enc of
            Nothing -> printf "No Encounter\n"
            Just (species, level, dv1, dv2) -> printf "Species: %d\tLevel: %d\tDVs: %02x%02x\n" species level dv1 dv2

runPath :: GB -> IORef Input -> [[Input]] -> IO (Location, Maybe (Word8, Word8, Word8, Word8))
runPath gb inputRef segPaths =
    case segPaths of
        [path1, path2, path3, path4, path5, path6, path7, path8, path9, path10, path11, path12, path13, path14, path15, path16, path17] -> do
            bufferedWalk gb inputRef (path1 ++ [i_Up])
            writeIORef inputRef i_A
            waitForItemJingle gb

            bufferedWalk gb inputRef (path2 ++ [i_Up])
            writeIORef inputRef i_A
            waitForItemJingle gb

            bufferedWalk gb inputRef path3
            bufferedWalk gb inputRef path4
            bufferedWalk gb inputRef path5
            bufferedWalk gb inputRef (path6 ++ [i_Up, i_Up, i_Left, i_Up, i_Right])
            writeIORef inputRef i_A
            waitForItemJingle gb

            bufferedWalk gb inputRef path7
            bufferedWalk gb inputRef path8
            bufferedWalk gb inputRef (path9 ++ replicate 5 i_Left)
            bufferedWalk gb inputRef (path10 ++ [i_Left])
            writeIORef inputRef i_A
            waitForItemJingle gb

            bufferedWalk gb inputRef path11
            bufferedWalk gb inputRef path12
            bufferedWalk gb inputRef path13
            bufferedWalk gb inputRef path14
            bufferedWalk gb inputRef path15
            bufferedWalk gb inputRef path16
            bufferedWalk gb inputRef path17
            
            loc <- getLocation gb
            encountered <- (/= 0) <$> cpuRead gb wIsInBattle
            encData <- if encountered
                then do
                    species <- cpuRead gb wEnemyMonSpecies
                    level <- cpuRead gb wEnemyMonLevel
                    dv1 <- cpuRead gb wEnemyMonAtkDefDV
                    dv2 <- cpuRead gb wEnemyMonSpdSpcDV
                    pure $ Just (species, level, dv1, dv2)
                else pure Nothing
            pure (loc, encData)
        _ -> error "Incorrect number of segments"

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''
