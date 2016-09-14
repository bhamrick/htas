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
    dat <- BS.readFile "pokered_r3_lass.sav"

    inputRef <- newIORef mempty

    for_ [0..59] $ \frame -> do
        loadSaveData gb (setSaveFrames frame dat)
        (loc, encData) <- r3LassManip gb inputRef
        printf "IGT0: %2d\t%s\t" frame (show loc)
        case encData of
            Nothing -> do
                printf "No encounter\n"
            Just (species, level, dv1, dv2) -> do
                printf "Species: %d\tLevel: %d\tDVs: %02x%02x\n" species level dv1 dv2

    {-
    setInputGetter gb (readIORef inputRef)

    printf "Creating initial states"
    initialStateGroup <- fmap catMaybes . for [0..59] $ \frame -> do
        printf "."
        loadSaveData gb (setSaveFrames frame dat)
        startSegments gb inputRef
    printf "\n"

    searchLoop gb inputRef initialStateGroup
    -}

r3LassManip :: GB -> IORef Input -> IO (Location, Maybe (Word8, Word8, Word8, Word8))
r3LassManip gb inputRef = do
    reset gb
    doOptimalIntro gb
    setInputGetter gb (readIORef inputRef)
    clearTraceCallback gb

    -- Automated search path
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Right, 1)
        , (i_Down, 1)
        , (i_Right, 5)
        , (i_Up, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 5)
        , (i_Right, 1)
        , (i_Up, 3)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Left, 1)
        , (i_Up, 15)
        , (i_Left, 2)
        , (i_Up, 5)
        , (i_Left, 2)
        , (i_Up, 1)
        , (i_Left, 2)
        -- Past lass
        , (i_Down, 1)
        , (i_Left, 4)
        , (i_Down, 1)
        , (i_Left, 5)
        , (i_Down, 1)
        , (i_Left, 1)
        , (i_Down, 1)
        , (i_Left, 2)
        , (i_Down, 4)
        -- B1F
        , (i_Left, 1)
        , (i_Down, 2)
        , (i_Left, 7)
        -- B2F
        , (i_Right, 1)
        , (i_Up, 1)
        , (i_Right, 2)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Right, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Down, 3)
        , (i_Left, 2)
        , (i_Down, 1)
        , (i_Left, 1)
        -- B1F
        , (i_Up, 1)
        , (i_Right, 6)
        , (i_Up, 1)
        , (i_Right, 2)
        -- 1F
        , (i_Down, 1)
        , (i_Left, 1)
        , (i_Down, 5)
        , (i_Left, 5)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 4)
        , (i_Left, 1)
        , (i_Up, 3)
        , (i_Left, 1)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Left, 2)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Down, 1)
        , (i_Right, 1)
        , (i_Down, 1)
        , (i_Right, 1)
        , (i_Down, 1)
        -- B1F
        , (i_Down, 3)
        , (i_Right, 1)
        , (i_Down, 6)
        , (i_Right, 1)
        , (i_Down, 2)
        , (i_Right, 10)
        , (i_Down, 1)
        , (i_Right, 4)
        -- B2F
        , (i_Right, 1)
        , (i_Up, 2)
        , (i_Right, 1)
        , (i_Up, 1)
        , (i_Right, 3)
        , (i_Right, 1)
        , (i_Down, 2)
        , (i_Right, 5)
        , (i_Up, 2)
        , (i_Right, 4)
        , (i_Down, 8)
        , (i_Left, 3)
        , (i_Down, 2)
        , (i_Left, 1)
        , (i_Down, 7)
        , (i_Left, 21)
        -- Freedom
        , (i_Left, 1)
        , (i_Up, 3)
        , (i_Up {-<> i_A-}, 1)
        , (i_Up, 4)
        , (i_Up {-<> i_A-}, 1)
        , (i_Up, 5)
        ]
    loc <- getLocation gb
    encountered <- (/= 0) <$> cpuRead gb wIsInBattle
    encData <- if encountered
        then Just <$> getEncounterData gb
        else pure Nothing
    pure $ (loc, encData)

startSegments :: GB -> IORef Input -> IO (Maybe ByteString)
startSegments gb inputRef = do
    reset gb
    doOptimalIntro gb
    setInputGetter gb (readIORef inputRef)
    clearTraceCallback gb

    -- Automated search path
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Right, 1)
        , (i_Down, 1)
        , (i_Right, 5)
        , (i_Up, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 5)
        , (i_Right, 1)
        , (i_Up, 3)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Left, 1)
        , (i_Up, 15)
        , (i_Left, 2)
        , (i_Up, 5)
        , (i_Left, 2)
        , (i_Up, 1)
        , (i_Left, 2)
        -- Past lass
        , (i_Down, 1)
        , (i_Left, 4)
        , (i_Down, 1)
        , (i_Left, 5)
        , (i_Down, 1)
        , (i_Left, 1)
        , (i_Down, 1)
        , (i_Left, 2)
        , (i_Down, 4)
        -- B1F
        , (i_Left, 1)
        , (i_Down, 2)
        , (i_Left, 7)
        -- B2F
        , (i_Right, 1)
        , (i_Up, 1)
        , (i_Right, 2)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Right, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Down, 3)
        , (i_Left, 2)
        , (i_Down, 1)
        , (i_Left, 1)
        -- B1F
        , (i_Up, 1)
        , (i_Right, 6)
        , (i_Up, 1)
        , (i_Right, 2)
        -- 1F
        , (i_Down, 1)
        , (i_Left, 1)
        , (i_Down, 5)
        , (i_Left, 5)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 4)
        , (i_Left, 1)
        , (i_Up, 3)
        , (i_Left, 1)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Left, 2)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Down, 1)
        , (i_Right, 1)
        , (i_Down, 1)
        , (i_Right, 1)
        , (i_Down, 1)
        -- B1F
        , (i_Down, 3)
        , (i_Right, 1)
        , (i_Down, 6)
        , (i_Right, 1)
        , (i_Down, 2)
        , (i_Right, 10)
        , (i_Down, 1)
        , (i_Right, 4)
        -- B2F
        , (i_Right, 1)
        , (i_Up, 2)
        , (i_Right, 1)
        , (i_Up, 1)
        , (i_Right, 3)
        , (i_Right, 1)
        , (i_Down, 2)
        , (i_Right, 5)
        , (i_Up, 2)
        , (i_Right, 4)
        , (i_Down, 8)
        , (i_Left, 3)
        , (i_Down, 2)
        , (i_Left, 1)
        , (i_Down, 7)
        , (i_Left, 21)
        -- Freedom
        , (i_Left, 1)
        , (i_Up, 3)
        , (i_Up <> i_A, 1)
        , (i_Up, 4)
        , (i_Up <> i_A, 1)
        , (i_Up, 5)
        ]

    {-
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Right, 1)
        , (i_Down, 1)
        , (i_Right, 5)
        , (i_Up, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 2)
        , (i_Right, 1)
        , (i_Up, 6)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 5)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Left, 1)
        , (i_Up, 7)
        , (i_Left, 1)
        , (i_Up, 3)
        , (i_Left, 1)
        , (i_Up, 5)
        , (i_Left, 3)
        -- After passing lass
        , (i_Left, 2)
        , (i_Down, 2)
        , (i_Left, 6)
        , (i_Down, 2)
        , (i_Left, 4)
        , (i_Down, 4)
        -- B1F
        , (i_Left, 4)
        , (i_Down, 1)
        , (i_Left, 1)
        , (i_Left <> i_A, 1)
        , (i_Left, 2)
        , (i_Down, 1)
        -- B2F
        , (i_Right, 1)
        , (i_Up, 1)
        , (i_Right, 2)
        , (i_Up, 2)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Right, 1)
        ]
    writeIORef inputRef i_A
    waitForItemJingle gb
    -}

    inBattle <- cpuRead gb wIsInBattle
    if inBattle /= 0
    then do
        loc <- getLocation gb
        encData <- getEncounterData gb
        pure Nothing
    else Just <$> saveState gb

segmentPaths :: Vector [Input]
segmentPaths =
    fmap (\b -> if b then i_Up else i_Left) <$>
        enumeratePaths 0 14 [(0, 10)]

applyPath :: GB -> IORef Input -> [Input] -> IO (Maybe (Location, (Word8, Word8, Word8, Word8)), (Word8, Word8))
applyPath gb inputRef path = do
    bufferedWalk gb inputRef (path)

    encountered <- (/=0) <$> cpuRead gb wIsInBattle
    if encountered
    then do
        loc <- getLocation gb
        encData <- getEncounterData gb
        hAdd <- cpuRead gb 0xFFD3
        hSub <- cpuRead gb 0xFFD4
        pure $ (Just (loc, encData), (hAdd, hSub))
    else do
        hAdd <- cpuRead gb 0xFFD3
        hSub <- cpuRead gb 0xFFD4
        pure $ (Nothing, (hAdd, hSub))

searchLoop :: GB -> IORef Input -> [ByteString] -> IO ()
searchLoop gb inputRef states = do
    path <- selectRandom segmentPaths >>= addAPresses >>= addAPresses
    printf "%s" (show path)
    results <- for states $ \state -> do
        printf "."
        loadState gb state
        applyPath gb inputRef path
    printf "\n"
    for_ (Map.toList (countMap results)) $ \(v, c) -> do
        printf "%s\t%d\n" (show v) c
    searchLoop gb inputRef states

countMap :: Ord a => [a] -> Map a Int
countMap = Map.fromListWith (+) . map (\x -> (x, 1))

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
