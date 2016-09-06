{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HTas.Direct where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data GB_Obj
type GB = Ptr GB_Obj

foreign import ccall "gambatte_create" gambatte_create :: IO GB

foreign import ccall "gambatte_load" gambatte_load :: GB -> CString -> CUInt -> CLLong -> CUInt -> IO CInt

gambatte_load' :: GB -> CString -> CUInt -> IO CInt
gambatte_load' gb rom romlength = gambatte_load gb rom romlength 0 0

foreign import ccall "gambatte_runfor" gambatte_runfor :: GB -> Ptr CShort -> Ptr CUInt -> IO CInt

foreign import ccall "gambatte_iscgb" gambatte_iscgb :: GB -> IO CInt

foreign import ccall "gambatte_isloaded" gambatte_isloaded :: GB -> IO CInt

foreign import ccall "gambatte_reset" gambatte_reset :: GB -> CLLong -> IO ()

gambatte_reset' :: GB -> IO ()
gambatte_reset' gb = gambatte_reset gb 0

foreign import ccall "gambatte_getmemoryarea" gambatte_getmemoryarea :: GB -> CInt -> Ptr (Ptr CChar) -> Ptr CInt -> IO Bool

foreign import ccall "gambatte_getregs" gambatte_getregs :: GB -> Ptr CInt -> IO ()

foreign import ccall "gambatte_getcyclecount" gambatte_getcyclecount :: GB -> IO CLong

foreign import ccall "gambatte_setreadcallback" gambatte_setreadcallback :: GB -> FunPtr (CUInt -> IO ()) -> IO ()

foreign import ccall "gambatte_setwritecallback" gambatte_setwritecallback :: GB -> FunPtr (CUInt -> IO ()) -> IO ()

foreign import ccall "gambatte_setexeccallback" gambatte_setexeccallback :: GB -> FunPtr (CUInt -> IO ()) -> IO ()

foreign import ccall "wrapper" createCallback :: (CUInt -> IO ()) -> IO (FunPtr (CUInt -> IO ()))

foreign import ccall "gambatte_cpuread" gambatte_cpuread :: GB -> CUShort -> IO Word8

foreign import ccall "gambatte_settracecallback" gambatte_settracecallback :: GB -> FunPtr (Ptr () -> IO ()) -> IO ()

foreign import ccall "wrapper" createTraceCallback :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

foreign import ccall "gambatte_loadsavedata" gambatte_loadsavedata :: GB -> CString -> IO ()

foreign import ccall "wrapper" createInputGetter :: IO CUInt -> IO (FunPtr (IO CUInt))

foreign import ccall "gambatte_setinputgetter" gambatte_setinputgetter :: GB -> FunPtr (IO CUInt) -> IO ()
