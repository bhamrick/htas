module MoonManip where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Random
import Text.Printf

import Red.Battle
import Search
import HTas.Direct
import HTas.Low

import Red.Overworld

enumeratePaths :: Int -> Int -> [(Int, Int)] -> Vector [Bool]
enumeratePaths start end bounds = Vector.fromList (go start end bounds)
    where
    go start end bounds =
        case bounds of
            [] -> pure $ replicate (end-start) True
            (lo,hi):rest -> do
                mid <- [max start lo .. hi]
                fmap ((replicate (mid - start) True ++ [False]) ++) (go mid end rest)

selectRandom :: Vector a -> IO a
selectRandom v = do
    i <- randomRIO (0, length v - 1)
    pure $ v Vector.! i

addAPresses :: [Input] -> IO [Input]
addAPresses inps = do
    shouldAdd <- coin 0.2
    if shouldAdd
    then do
        aIdx <- randomRIO (1, length inps - 1)
        let (start, end) = splitAt aIdx inps
        case end of
            [] -> pure start
            i:is -> pure $ start ++ ((i <> i_A) : is)
    else pure inps

type StateGroup = [ByteString]

r3LassSegments :: GB -> IORef Input -> IORef Bool -> [Segment StateGroup [Input]]
r3LassSegments gb inputRef encounterRef =
    [ let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 6 [(0, 6)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 1: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Up])
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Up else i_Right)
            <$> enumeratePaths 0 7 [(0, 7)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 2: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Up])
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 21 [(0, 21), (0, 21), (13, 21), (13, 21), (13, 21), (21, 21), (21, 21)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 3: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Down else i_Left)
            <$> enumeratePaths 0 8 [(0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4), (0, 4)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 4: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 8 [(0, 8), (0, 8)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 5: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 3 [(0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 6: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Up, i_Up, i_Left, i_Up, i_Right])
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Left else i_Down)
            <$> enumeratePaths 0 3 [(0, 0), (0, 0), (0, 0), (0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 7: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 8 [(0, 8), (0, 8)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 8: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Down else i_Left)
            <$> enumeratePaths 0 6 [(0, 6)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 9: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Left, i_Left, i_Left, i_Left, i_Left])
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 15 [(0, 15), (0, 15), (0, 15), (0, 15), (0, 15), (13, 15), (13, 15)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 10: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef (path ++ [i_Left])
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else do
                    writeIORef inputRef i_A
                    waitForItemJingle gb
                    Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Down else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 11: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B1F after moon stone
    , let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 16 [(0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 2), (0, 16)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 12: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    -- B2F after moon stone
    , let
        paths = fmap (\b -> if b then i_Up else i_Right)
            <$> enumeratePaths 0 3 [(0, 3), (0, 3), (3, 3), (3, 3), (3, 3)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 13: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Right else i_Down)
            <$> enumeratePaths 0 6 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 14: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Right else i_Up)
            <$> enumeratePaths 0 4 [(0, 1), (0, 1)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 15: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Down else i_Left)
            <$> enumeratePaths 0 17 [(8, 10), (8, 10), (8, 17), (8, 17)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 16: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = Vector.fromList [replicate 21 i_Left]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 17: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then pure Nothing
                else Just <$> saveState gb
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    , let
        paths = fmap (\b -> if b then i_Up else i_Left)
            <$> enumeratePaths 0 14 [(0, 10)]
      in Segment
        { generate = selectRandom paths >>= addAPresses
        , apply = \path stateGroup -> do
            printf "Segment 18: %s to %d states" (show path) (length stateGroup)
            resultMaybeStates <- for stateGroup $ \state -> do
                printf "."
                writeIORef encounterRef False
                loadState gb state

                bufferedWalk gb inputRef path
                encountered <- readIORef encounterRef
                if encountered
                then do
                    advanceUntil gb ((/= 0) <$> cpuRead gb wIsInBattle)
                    species <- cpuRead gb wEnemyMonSpecies
                    level <- cpuRead gb wEnemyMonLevel
                    if species == 109 && level == 10
                    then Just <$> saveState gb
                    else pure Nothing
                else pure Nothing
            let resultStates = catMaybes resultMaybeStates
            printf "%d states remain\n" (length resultStates)
            pure $ (resultStates, fromIntegral (length resultStates))
        }
    ]
