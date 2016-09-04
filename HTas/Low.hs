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

data TraceData = TraceData
    { trace_cycle :: Int
    , trace_PC :: Int
    , trace_SP :: Int
    , trace_A :: Int
    , trace_B :: Int
    , trace_C :: Int
    , trace_D :: Int
    , trace_E :: Int
    , trace_F :: Int
    , trace_H :: Int
    , trace_L :: Int
    , trace_skip :: Int
    , trace_opcode :: Int
    , trace_LY :: Int
    } deriving (Eq, Show, Ord)

instance Storable TraceData where
    sizeOf _ = 14 * sizeOf (undefined :: CInt)
    alignment _ = 1
    peek p = do
        let p' = castPtr p :: Ptr CInt
        cycle <- peekElemOff p' 0
        pc <- peekElemOff p' 1
        sp <- peekElemOff p' 2
        a <- peekElemOff p' 3
        b <- peekElemOff p' 4
        c <- peekElemOff p' 5
        d <- peekElemOff p' 6
        e <- peekElemOff p' 7
        f <- peekElemOff p' 8
        h <- peekElemOff p' 9
        l <- peekElemOff p' 10
        skip <- peekElemOff p' 11
        opcode <- peekElemOff p' 12
        ly <- peekElemOff p' 13
        pure $ TraceData
            { trace_cycle = fromIntegral cycle
            , trace_PC = fromIntegral pc
            , trace_SP = fromIntegral sp
            , trace_A = fromIntegral a
            , trace_B = fromIntegral b
            , trace_C = fromIntegral c
            , trace_D = fromIntegral d
            , trace_E = fromIntegral e
            , trace_F = fromIntegral f
            , trace_H = fromIntegral h
            , trace_L = fromIntegral l
            , trace_skip = fromIntegral skip
            , trace_opcode = fromIntegral opcode
            , trace_LY = fromIntegral ly
            }
    poke p dat = do
        let p' = castPtr p :: Ptr CInt
        pokeElemOff p' 0 (fromIntegral $ trace_cycle dat)
        pokeElemOff p' 1 (fromIntegral $ trace_PC dat)
        pokeElemOff p' 2 (fromIntegral $ trace_SP dat)
        pokeElemOff p' 3 (fromIntegral $ trace_A dat)
        pokeElemOff p' 4 (fromIntegral $ trace_B dat)
        pokeElemOff p' 5 (fromIntegral $ trace_C dat)
        pokeElemOff p' 6 (fromIntegral $ trace_D dat)
        pokeElemOff p' 7 (fromIntegral $ trace_E dat)
        pokeElemOff p' 8 (fromIntegral $ trace_F dat)
        pokeElemOff p' 9 (fromIntegral $ trace_H dat)
        pokeElemOff p' 10 (fromIntegral $ trace_L dat)
        pokeElemOff p' 11 (fromIntegral $ trace_skip dat)
        pokeElemOff p' 12 (fromIntegral $ trace_opcode dat)
        pokeElemOff p' 13 (fromIntegral $ trace_LY dat)

setTraceCallback :: GB -> (TraceData -> IO ()) -> IO ()
setTraceCallback gb cb = do
    cCallback <- createTraceCallback cb'
    gambatte_settracecallback gb cCallback
    where
    cb' :: Ptr () -> IO ()
    cb' p = peek (castPtr p) >>= cb

traceRegs :: TraceData -> Regs
traceRegs dat =
    Regs
        { reg_PC = trace_PC dat
        , reg_SP = trace_SP dat
        , reg_A = trace_A dat
        , reg_B = trace_B dat
        , reg_C = trace_C dat
        , reg_D = trace_D dat
        , reg_E = trace_E dat
        , reg_F = trace_F dat
        , reg_H = trace_H dat
        , reg_L = trace_L dat
        }

loadSaveData :: GB -> ByteString -> IO ()
loadSaveData gb dat = do
    BS.useAsCString dat $ \cDat ->
        gambatte_loadsavedata gb cDat
