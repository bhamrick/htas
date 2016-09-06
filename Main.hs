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
    dat <- BS.readFile "pokered_moon_bc_nerd.sav"

    for_ [0..59] $ \frame -> do
        loadSaveData gb (setSaveFrames frame dat)
        (loc, enc) <- moonBcNerdManip gb
        printf "IGT0: %2d\t%s\t" frame (show loc)
        case enc of
            Nothing -> printf "No encounter\n"
            Just (species, level, dv1, dv2) -> printf "Species: %d\tLevel: %d\tDVs: %02x%02x\n" species level dv1 dv2

setSaveFrames :: Word8 -> ByteString -> ByteString
setSaveFrames f dat =
    let dat' = editByte saveTimeFrames f dat
        checksum = computeChecksum saveMainDataStart saveMainDataEnd dat'
        dat'' = editByte saveMainDataChecksum checksum dat'
    in
    dat''

moonBcNerdManip :: GB -> IO (Location, Maybe (Word8, Word8, Word8, Word8))
moonBcNerdManip gb = do
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
        [ (i_Down, 1)
        , (i_Right, 6)
        , (i_Up, 1)
        ]
    encountered <- readIORef encounterRef
    when (not encountered) $ do
        writeIORef inputRef (i_Up <> i_A)
        waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 4)
        , (i_Right, 1)
        , (i_Up, 4)
        ]
    encountered <- readIORef encounterRef
    when (not encountered) $ do
        writeIORef inputRef (i_Up <> i_A)
        waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Up, 21)
        , (i_Left, 12)
        , (i_Down, 4)
        , (i_Left, 7)
        , (i_Down, 4)
        -- B1F
        , (i_Down, 2)
        , (i_Left, 8)
        -- B2F (Mega punch)
        , (i_Right, 3)
        , (i_Up, 3)
        , (i_Left, 1)
        , (i_Up, 1)
        , (i_Right, 1)
        ]
    encountered <- readIORef encounterRef
    when (not encountered) $ do
        writeIORef inputRef (i_Right <> i_A)
        waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Down, 4)
        , (i_Left, 3)
        -- B1F
        , (i_Right, 8)
        , (i_Up, 2)
        -- 1F
        , (i_Down, 6)
        , (i_Left, 6)
        , (i_Up, 15)
        , (i_Left, 8)
        ]
    encountered <- readIORef encounterRef
    when (not encountered) $ do
        writeIORef inputRef (i_Left <> i_A)
        waitForItemJingle gb
    bufferedWalk gb inputRef . rleExpand $
        [ (i_Right, 2)
        , (i_Down, 3)
        -- B1F
        , (i_Down, 12)
        , (i_Right, 16)
        -- B2F
        , (i_Up, 3)
        , (i_Right, 5)
        , (i_Down, 2)
        , (i_Right, 7)
        , (i_Up, 2)
        , (i_Right, 3)
        , (i_Down, 8)
        , (i_Left, 2)
        , (i_Down, 9)
        , (i_Left <> i_A, 1)
        , (i_Left, 20)
        , (i_Left <> i_A, 1)
        , (i_Left, 2)
        , (i_Up <> i_A, 1)
        , (i_Up, 14)
        , (i_Right, 1)
        ]

    loc <- getLocation gb 
    encountered <- readIORef encounterRef
    encData <- if encountered
        then do
            advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
            species <- cpuRead gb wEnemyMonSpecies
            level <- cpuRead gb wEnemyMonLevel
            dv1 <- cpuRead gb wEnemyMonAtkDefDV
            dv2 <- cpuRead gb wEnemyMonSpdSpcDV
            pure $ Just (species, level, dv1, dv2)
        else pure Nothing
    pure (loc, encData)

fbeeManip :: GB -> IO (Word8, Word8, Word8, Word8)
fbeeManip gb = do
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
        [ (i_Left, 8)
        , (i_Right, 3)
        , (i_Left, 7)
        , (i_Down, 3)
        , (i_Left, 2)
        , (i_Up, 4)
        , (i_Down, 3)
        ] <> cycle [(i_Up, 3), (i_Down, 3)]
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
