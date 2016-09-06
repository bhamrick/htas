module Red.Intro (doOptimalIntro) where

import Control.Monad
import Data.IORef
import Data.Monoid
import Text.Printf

import HTas.Low
import HTas.Direct

data IntroState
    = Start
    | Credits
    | TitleScreen
    | MainMenu
    | Done
    deriving (Eq, Show, Ord)

doOptimalIntro :: GB -> IO ()
doOptimalIntro gb = do
    introState <- newIORef Start
    frameCounter <- newIORef (0 :: Integer)

    setInputGetter gb $ do
        s <- readIORef introState
        pure $ case s of
            Start -> mempty
            Credits -> i_Up <> i_B <> i_Select
            TitleScreen -> i_Start
            MainMenu -> i_A
            Done -> mempty

    setTraceCallback gb $ \dat -> do
        let addr = trace_PC dat
        frame <- readIORef frameCounter
        curState <- readIORef introState
        case curState of
            Start -> do
                when (addr == 0x589D) $ do
                    writeIORef introState Credits
            Credits -> do
                when (addr == 0x42DD) $ do
                    writeIORef introState TitleScreen
            TitleScreen -> do
                when (addr == 0x5AF2) $ do
                    writeIORef introState MainMenu
            MainMenu -> do
                when (addr == 0x5D52) $ do
                    writeIORef introState Done
                when (addr == 0x5BD1) $ do
                    writeIORef introState Done
            Done -> do
                pure ()

    advanceUntilDone gb introState frameCounter

advanceUntilDone :: GB -> IORef IntroState -> IORef Integer -> IO ()
advanceUntilDone gb stateRef frameCounter = do
    s <- readIORef stateRef
    if s == Done
    then pure ()
    else do
        advanceFrame gb
        modifyIORef' frameCounter (+1)
        advanceUntilDone gb stateRef frameCounter
