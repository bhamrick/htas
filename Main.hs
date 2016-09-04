module Main where

import HTas.Direct

main :: IO ()
main = do
    gb <- gambatte_create
    loaded <- gambatte_isloaded gb
    print loaded
