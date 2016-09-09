module MoonManip where

import Data.IORef

import Search
import HTas.Direct
import HTas.Low

enumeratePaths :: Int -> Int -> [(Int, Int)] -> [[Int]]
enumeratePaths start end bounds =
    case bounds of
        [] -> pure $ replicate (end-start) 1
        (lo,hi):rest -> do
            mid <- [max start lo .. hi]
            fmap ((replicate (mid - start) 1 ++ [0]) ++) (enumeratePaths mid end rest)
