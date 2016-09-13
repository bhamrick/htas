module Search where

import System.Random
import Text.Printf

data Segment a b = Segment
    { generate :: IO b
    , apply :: b -> a -> IO (a, Double)
    }

data Checkpoint a b = Checkpoint
    { revPaths :: [b]
    , currentState :: a
    , futureSegments :: [Segment a b]
    , value :: Double
    , segmentCount :: Integer
    }

data SearchState a b = SearchState
    { initialState :: a
    , segments :: [Segment a b]
    , checkpoints :: [Checkpoint a b]
    }

baseReplaceProbability :: Checkpoint a b -> Checkpoint a b -> Double
baseReplaceProbability oldCheck newCheck =
    let seg1 = fromInteger (segmentCount oldCheck)
        seg2 = fromInteger (segmentCount newCheck)
        val1 = value oldCheck
        val2 = value newCheck
    in
    if val2 == 0 then 0
    else min 1 $ exp (0.2*(seg2-seg1) - val1/val2)

coin :: Double -> IO Bool
coin p = (< p) <$> randomRIO (0, 1)

index :: Int -> [a] -> Maybe a
index n l =
    case l of
        [] -> Nothing
        a:as ->
            if n == 0
            then Just a
            else index (n-1) as

setIndex :: Int -> a -> [a] -> [a]
setIndex n a l =
    let
    (start, end) = splitAt n l
    in
    case end of
        [] -> start
        _:as -> start ++ (a:as)

forFirst :: [a] -> (a -> IO (Maybe b)) -> IO (Maybe b)
forFirst l f =
    case l of
        [] -> pure Nothing
        a:as -> do
            mb <- f a
            case mb of
                Nothing -> forFirst as f
                Just b -> pure (Just b)

forFirstWithIndex :: [a] -> (Int -> a -> IO (Maybe b)) -> IO (Maybe b)
forFirstWithIndex l f = go l f 0
    where
    go l f n =
        case l of
            [] -> pure Nothing
            a:as -> do
                mb <- f n a
                case mb of
                    Nothing -> (n+1) `seq` go as f (n+1)
                    Just b -> pure (Just b)

newCheckpoint :: SearchState a b -> IO (Maybe (Checkpoint a b))
newCheckpoint ss = do
    let maxN = length (checkpoints ss)
    sourceCheck <- forFirstWithIndex (checkpoints ss) $ \i check -> do
        shouldUse <- coin (1/(fromIntegral maxN + 1 - fromIntegral i))
        if shouldUse
        then pure $ Just check
        else pure Nothing
    case sourceCheck of
        Nothing -> do
            -- Generate from initial state
            case segments ss of
                [] -> pure Nothing
                seg:rest -> do
                    path <- generate seg 
                    (endState, val) <- apply seg path (initialState ss)
                    pure . Just $ Checkpoint
                        { revPaths = [path]
                        , currentState = endState
                        , futureSegments = rest
                        , value = val
                        , segmentCount = 1
                        }
        Just check -> do
            -- Generate from intermediate state
            case futureSegments check of
                [] -> pure Nothing
                seg:rest -> do
                    path <- generate seg
                    (endState, val) <- apply seg path (currentState check)
                    pure . Just $ Checkpoint
                        { revPaths = path:(revPaths check)
                        , currentState = endState
                        , futureSegments = rest
                        , value = val
                        , segmentCount = segmentCount check + 1
                        }

considerCheckpoint :: Checkpoint a b -> SearchState a b -> IO (SearchState a b)
considerCheckpoint check ss = do
    if value check > 0
    then do
        let maybeOldCheck = index (fromInteger $ segmentCount check - 1) (checkpoints ss)
        case maybeOldCheck of
            Nothing -> pure $ ss { checkpoints = checkpoints ss ++ [check] }
            Just oldCheck -> do
                shouldReplace <- coin $ baseReplaceProbability oldCheck check
                if shouldReplace
                then pure $ ss { checkpoints = setIndex (fromInteger $ segmentCount check - 1) check (checkpoints ss) }
                else pure $ ss
    else pure ss

searchIteration :: SearchState a b -> IO (SearchState a b)
searchIteration ss = do
    mCheck <- newCheckpoint ss
    case mCheck of
        Nothing -> pure ss
        Just c -> considerCheckpoint c ss

iterateM :: Int -> (a -> IO a) -> a -> IO a
iterateM n f a =
    if n == 0
    then pure a
    else f a >>= iterateM (n-1) f
