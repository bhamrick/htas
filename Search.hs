module Search where

import System.Random
import Text.Printf

data Segment a b = Segment
    { generate :: IO b
    , apply :: b -> a -> IO (a, Double)
    }

data SearchState a b =
    SearchState
        { segment :: Segment a b
        , initialState :: a
        , currentPath :: b
        , currentResult :: (a, Double)
        }

-- Search parameters
reconsiderProbability = 0.5
backtrackProbability = 0.1
replacementRatio = 1
replacementProbability p q = if q == 0 then 0 else exp (-replacementRatio * p / q)
acceptRatio = 1
acceptProbability v = 1 - exp (-acceptRatio * v)

coin :: Double -> IO Bool
coin p = (< p) <$> randomRIO (0, 1)

search :: Show b => [SearchState a b] -> a -> [Segment a b] -> IO [SearchState a b]
search lead a segs =
    reverse <$> go (reverse lead) a segs
    where
    go acc a segs =
        case segs of
            [] -> pure acc
            s:ss -> do
                path <- generate s
                (a', v) <- apply s path a
                accept <- coin (acceptProbability v)
                if accept
                then do
                    printf "Accepting %s (value %f)\n" (show path) v
                    go
                        (SearchState
                            { segment = s
                            , initialState = a
                            , currentPath = path
                            , currentResult = (a', v)
                            } : acc
                        )
                        a'
                        ss
                else do
                    shouldReconsider <- coin reconsiderProbability
                    if shouldReconsider 
                    then do
                        printf "Reconsidering\n"
                        case acc of
                            [] -> go acc a segs
                            (st:sts) -> reconsider sts st segs
                    else do
                        go acc a segs
    reconsider acc st segs = do
        let (a', val') = currentResult st
        newPath <- generate (segment st)
        (a'', val'') <- apply (segment st) newPath (initialState st)
        replace <- coin (replacementProbability val' val'')
        if replace
        then do
            printf "Updating to %s (value %f)\n" (show newPath) val''
            go (st { currentPath = newPath, currentResult = (a'', val'') } : acc) a'' segs
        else do
            backtrack <- coin backtrackProbability
            if backtrack
            then do
                printf "Backtracking\n"
                case acc of
                    [] -> reconsider acc st segs
                    (st':sts) -> reconsider sts st' (segment st:segs)
            else do
                reconsider acc st segs
