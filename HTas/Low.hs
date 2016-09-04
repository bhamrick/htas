{-# LANGUAGE BangPatterns #-}
module HTas.Low where

-- Low level abstractions over the direct interface

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign hiding (void)
import Foreign.C.Types
import HTas.Direct
import System.FilePath
import Text.Printf

create :: IO GB
create = gambatte_create

loadRomFile :: GB -> FilePath -> IO Int
loadRomFile gb path = do
    romData <- BS.readFile path
    ret <- BS.useAsCStringLen romData $ \(cRomData, len) -> do
        gambatte_load' gb cRomData (fromIntegral len)
    pure (fromIntegral ret)

data MemoryArea
    = VRAM
    | ROM
    | WRAM
    | CARTRAM
    | OAM
    | HRAM
    deriving (Eq, Show, Ord)

memoryAreaCode :: MemoryArea -> CInt
memoryAreaCode area =
    case area of
        VRAM -> 0
        ROM -> 1
        WRAM -> 2
        CARTRAM -> 3
        OAM -> 4
        HRAM -> 5

getMemoryArea :: GB -> MemoryArea -> IO ByteString
getMemoryArea gb area = do
    datPtr <- malloc
    lenPtr <- malloc
    void $ gambatte_getmemoryarea gb (memoryAreaCode area) datPtr lenPtr
    cDat <- peek datPtr
    cLen <- peek lenPtr
    dat <- BS.packCStringLen (cDat, fromIntegral cLen)
    free datPtr
    free lenPtr
    pure dat

data Regs = Regs
    { reg_PC :: Int
    , reg_SP :: Int
    , reg_A :: Int
    , reg_B :: Int
    , reg_C :: Int
    , reg_D :: Int
    , reg_E :: Int
    , reg_F :: Int
    , reg_H :: Int
    , reg_L :: Int
    } deriving (Eq, Show, Ord)

getRegs :: GB -> IO Regs
getRegs gb = do
    regArea <- mallocBytes (10 * sizeOf (undefined :: CInt))
    gambatte_getregs gb regArea
    pc <- peekElemOff regArea 0
    sp <- peekElemOff regArea 1
    a <- peekElemOff regArea 2
    b <- peekElemOff regArea 3
    c <- peekElemOff regArea 4
    d <- peekElemOff regArea 5
    e <- peekElemOff regArea 6
    f <- peekElemOff regArea 7
    h <- peekElemOff regArea 8
    l <- peekElemOff regArea 9
    free regArea
    pure $ Regs
        { reg_PC = fromIntegral pc
        , reg_SP = fromIntegral sp
        , reg_A = fromIntegral a
        , reg_B = fromIntegral b
        , reg_C = fromIntegral c
        , reg_D = fromIntegral d
        , reg_E = fromIntegral e
        , reg_F = fromIntegral f
        , reg_H = fromIntegral h
        , reg_L = fromIntegral l
        }

printRegs :: Regs -> IO ()
printRegs regs = do
    printf "af = %02x%02x\n" (reg_A regs) (reg_F regs)
    printf "bc = %02x%02x\n" (reg_B regs) (reg_C regs)
    printf "de = %02x%02x\n" (reg_D regs) (reg_E regs)
    printf "hl = %02x%02x\n" (reg_H regs) (reg_L regs)
    printf "sp = %04x\n" (reg_SP regs)
    printf "pc = %04x\n" (reg_PC regs)

advanceFrame :: GB -> IO ()
advanceFrame gb = do
    soundbuf <- mallocBytes ((35112+2064) * sizeOf (undefined :: CInt))
    samples <- new 35112
    ret <- gambatte_runfor gb soundbuf samples
    free soundbuf
    free samples
    when (ret < 0) (advanceFrame gb)

getCycleCount :: GB -> IO Integer
getCycleCount gb = do
    fromIntegral <$> gambatte_getcyclecount gb

setExecCallback :: GB -> (Integer -> IO ()) -> IO ()
setExecCallback gb cb = do
    cCallback <- createCallback (cb . toInteger)
    gambatte_setexeccallback gb cCallback

isCgb :: GB -> IO Integer
isCgb gb = toInteger <$> gambatte_iscgb gb

cpuRead :: GB -> Integer -> IO Word8
cpuRead gb addr = gambatte_cpuread gb (fromInteger addr)
